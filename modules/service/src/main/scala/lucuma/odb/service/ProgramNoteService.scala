// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.odb.data.Existence
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.ProgramNotePropertiesInput.Create
import lucuma.odb.graphql.input.ProgramNotePropertiesInput.Edit
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

trait ProgramNoteService[F[_]]:
  def createNote(
    input: AccessControl.CheckedWithId[Create, Program.Id]
  )(using Transaction[F]): F[Result[ProgramNote.Id]]

  def updateNotes(
    update: AccessControl.Checked[Edit]
  )(using Transaction[F]): F[List[ProgramNote.Id]]

object ProgramNoteService:

  def instantiate[F[_]: Concurrent : Trace](using Services[F]): ProgramNoteService[F] =

    new ProgramNoteService[F]:
      override def createNote(
        input: AccessControl.CheckedWithId[Create, Program.Id]
      )(using Transaction[F]): F[Result[ProgramNote.Id]] =
        Trace[F].span("createNote"):
          session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
          input.foldWithId(
            OdbError.InvalidArgument().asFailureF
          )((set, pid) => session.unique(Statements.InsertNote)(pid, set).map(_.success))

      override def updateNotes(
        update: AccessControl.Checked[Edit]
      )(using Transaction[F]): F[List[ProgramNote.Id]] =
        Trace[F].span("updateNotes"):
          update.fold(List.empty.pure[F]): (SET, which) =>
            Statements.updateNotes(SET, which).traverse: af =>
              session.prepareR(af.fragment.query(program_note_id)).use: pq =>
                pq.stream(af.argument, chunkSize = 1024).compile.toList
            .map(_.toList.flatten)

  object Statements:

    val InsertNote: Query[(Program.Id, Create), ProgramNote.Id] =
      sql"""
        INSERT INTO t_program_note (
          c_program_id,
          c_existence,
          c_title,
          c_text,
          c_private
        )
        VALUES (
          $program_id,
          $existence,
          $text_nonempty,
          ${text_nonempty.opt},
          $bool
        )
        RETURNING c_program_note_id
      """.query(program_note_id).contramap: (pid, input) =>
        (
          pid,
          input.existence,
          input.title,
          input.text,
          input.isPrivate
        )

    def updates(SET: Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.title.map(sql"c_title = $text_nonempty"),
          SET.text.foldPresent(sql"c_text = ${text_nonempty.opt}"),
          SET.isPrivate.map(sql"c_private = $bool"),
        ).flatten
      )

    def updateNotes(
      SET:   Edit,
      which: AppliedFragment
    ): Option[AppliedFragment] =

      updates(SET).map: ups =>
        void"UPDATE t_program_note "                              |+|
        void"SET " |+| ups.intercalate(void", ") |+| void" "      |+|
        void"WHERE c_program_note_id IN (" |+| which |+| void") " |+|
        void"RETURNING c_program_note_id"
