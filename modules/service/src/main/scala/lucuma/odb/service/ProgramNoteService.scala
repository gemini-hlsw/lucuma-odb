// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.odb.data.Existence
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.ProgramNotePropertiesInput.Create
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

  object Statements:

    val InsertNote: Query[(Program.Id, Create), ProgramNote.Id] =
      sql"""
        INSERT INTO t_program_note (
          c_program_id,
          c_existence,
          c_index,
          c_title,
          c_text,
          c_private
        )
        VALUES (
          $program_id,
          $existence,
          COALESCE((SELECT MAX(c_index) FROM t_program_note WHERE c_program_id = $program_id), 0) + 1,
          $text_nonempty,
          ${text_nonempty.opt},
          $bool
        )
        RETURNING c_program_note_id
      """.query(program_note_id).contramap: (pid, input) =>
        (
          pid,
          input.existence,
          pid,
          input.title,
          input.text,
          input.isPrivate
        )