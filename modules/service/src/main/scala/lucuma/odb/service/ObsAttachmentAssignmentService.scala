// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait ObsAttachmentAssignmentService[F[_]] {

  /**
   * Inserts (program, observation, Attachment) triplets covering all combinations. In other
   * words, every observation in `observationIds` will be given all the attachments in
   * `attachmentIds` in addition to any existing Attachments they may already have.
   */
  def insertAssignments(
    programId:        Program.Id,
    observationIds:   List[Observation.Id],
    attachmentIds:    List[Attachment.Id]
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Deletes the asterisms associated with the given observation ids.
   */
  def deleteAssignments(
    programId:      Program.Id,
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Replaces the existing asterisms associated with the given observation ids (if any) with the
   * given attachments. This is essentially a delete followed by an insert.
   */
  def setAssignments(
    programId:        Program.Id,
    observationIds:   List[Observation.Id],
    attachmentIds:    Nullable[List[Attachment.Id]]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneAssignments(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[Unit]
}

object ObsAttachmentAssignmentService {
  def ForeignKeyViolationMessage(
    programId:        Program.Id,
    attachmentIds:    NonEmptyList[Attachment.Id]
  ): String =
    s"Attachment(s) ${attachmentIds.map(_.show).intercalate(", ")} must exist and be associated with Program ${programId.show}."

  def NonObservationAttachmentMessage(attachmentIds: NonEmptyList[Attachment.Id]): String =
    s"One or more of the attachment(s) ${attachmentIds.map(_.show).intercalate(", ")} cannot be assigned to observations."

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ObsAttachmentAssignmentService[F] =
    new ObsAttachmentAssignmentService[F] {
      override def insertAssignments(
        programId:        Program.Id,
        observationIds:   List[Observation.Id],
        attachmentIds:    List[Attachment.Id]
      )(using Transaction[F]): F[Result[Unit]] = {
        (NonEmptyList.fromList(observationIds), NonEmptyList.fromList(attachmentIds)).mapN { (oids, aids) =>
        val af = Statements.insertLinksAs(user, programId, oids, aids)
        session.prepareR(af.fragment.command).use { p =>
          p.execute(af.argument)
            .as(Result.unit)
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) =>
                Result.failure(ForeignKeyViolationMessage(programId, aids)).pure[F]
              case SqlState.CheckViolation(e) if e.constraintName.exists(_.contains("only_obs_attachments_check")) =>
                Result.failure(NonObservationAttachmentMessage(aids)).pure[F]
            }
        }
        }.getOrElse(Result.unit.pure[F])
      }

      override def deleteAssignments(
        programId:      Program.Id,
        observationIds: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] = {
        NonEmptyList.fromList(observationIds).fold(Result.unit.pure[F]) { oids =>
          val af = Statements.deleteAllLinksAs(user, programId, oids)
          session.prepareR(af.fragment.command).use { p =>
            p.execute(af.argument).as(Result.unit)
          }
        }
      }

      override def setAssignments(
        programId:        Program.Id,
        observationIds:   List[Observation.Id],
        attachmentIds:    Nullable[List[Attachment.Id]]
    )(using Transaction[F]): F[Result[Unit]] =
      attachmentIds match {
          case Nullable.Null          => deleteAssignments(programId, observationIds)
          case Nullable.Absent        => Result.unit.pure[F]
          case Nullable.NonNull(aids) =>
            deleteAssignments(programId, observationIds) *>
              insertAssignments(programId, observationIds, aids)
        }

      override def cloneAssignments(originalId: Observation.Id, newId: Observation.Id)(using
        Transaction[F]
      ): F[Unit] = {
        val clone = Statements.clone(originalId, newId)
        session.prepareR(clone.fragment.command).use { p =>
          p.execute(clone.argument).void
        }
      }
    }

  object Statements {

    import ProgramUserService.Statements.{andWhereUserWriteAccess}

    def insertLinksAs(
      user:             User,
      programId:        Program.Id,
      observationIds:   NonEmptyList[Observation.Id],
      attachmentIds:    NonEmptyList[Attachment.Id]
    ): AppliedFragment =
      void"""
        INSERT INTO t_obs_attachment_assignment (
          c_program_id,
          c_observation_id,
          c_attachment_id,
          c_attachment_type
        )
        SELECT a.c_program_id, o.c_observation_id, a.c_attachment_id, a.c_attachment_type
        FROM t_attachment a, t_observation o
        WHERE """ |+| observationIdIn(observationIds) |+|
      void" AND " |+| attachmentIdIn(attachmentIds) |+|
      andWhereUserWriteAccess(user, programId) |+|
      void""" ON CONFLICT DO NOTHING""" // the key consists of all the columns, anyway

    private def programIdEqual(
      programId: Program.Id
    ): AppliedFragment =
      sql"c_program_id = $program_id" (programId)

    private def attachmentIdIn(
      attachmentIds: NonEmptyList[Attachment.Id]
    ): AppliedFragment =
      void"c_attachment_id IN (" |+|
        attachmentIds.map(sql"$attachment_id").intercalate(void", ") |+|
        void")"

    def deleteLinksAs(
      user:             User,
      programId:        Program.Id,
      observationIds:   NonEmptyList[Observation.Id],
      attachmentIds: NonEmptyList[Attachment.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_obs_attachment_assignment " |+|
        void"WHERE " |+| programIdEqual(programId) |+|
        void" AND " |+| observationIdIn(observationIds) |+|
        void" AND " |+| attachmentIdIn(attachmentIds) |+|
        andWhereUserWriteAccess(user, programId)

    def deleteAllLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_obs_attachment_assignment " |+|
        void"WHERE " |+| programIdEqual(programId) |+|
        void" AND " |+| observationIdIn(observationIds) |+|
        andWhereUserWriteAccess(user, programId)

    def clone(originalOid: Observation.Id, newOid: Observation.Id): AppliedFragment =
      sql"""
        INSERT INTO t_obs_attachment_assignment (
          c_program_id,
          c_observation_id,
          c_attachment_id,
          c_attachment_type
        )
        SELECT
          t_obs_attachment_assignment.c_program_id,
          $observation_id,
          t_obs_attachment_assignment.c_attachment_id,
          t_obs_attachment_assignment.c_attachment_type
        FROM t_obs_attachment_assignment
        JOIN t_attachment ON t_attachment.c_attachment_id = t_obs_attachment_assignment.c_attachment_id
        WHERE c_observation_id = $observation_id
      """.apply(newOid, originalOid)
  }
}
