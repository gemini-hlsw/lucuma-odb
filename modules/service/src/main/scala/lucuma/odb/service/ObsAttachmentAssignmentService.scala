// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.util.Codecs.obs_attachment_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait ObsAttachmentAssignmentService[F[_]] {

  /**
   * Inserts (program, observation, obsAttachment) triplets covering all combinations. In other
   * words, every observation in `observationIds` will be given all the obsAttachments in
   * `obsAttachmentIds` in addition to any existing obsAttachments they may already have.
   */
  def insertAssignments(
    programId:        Program.Id,
    observationIds:   List[Observation.Id],
    obsAttachmentIds: List[ObsAttachment.Id]
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
   * given obsAttachments. This is essentially a delete followed by an insert.
   */
  def setAssignments(
    programId:        Program.Id,
    observationIds:   List[Observation.Id],
    obsAttachmentIds: Nullable[List[ObsAttachment.Id]]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneAssignments(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[Unit]
}

object ObsAttachmentAssignmentService {
  def ForeignKeyViolationMessage(
    programId:        Program.Id,
    obsAttachmentIds: NonEmptyList[ObsAttachment.Id]
  ): String =
    s"ObsAttachment(s) ${obsAttachmentIds.map(_.show).intercalate(", ")} must exist and be associated with Program ${programId.show}."

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ObsAttachmentAssignmentService[F] =
    new ObsAttachmentAssignmentService[F] {
      override def insertAssignments(
        programId:        Program.Id,
        observationIds:   List[Observation.Id],
        obsAttachmentIds: List[ObsAttachment.Id]
      )(using Transaction[F]): F[Result[Unit]] = {
        (NonEmptyList.fromList(observationIds), NonEmptyList.fromList(obsAttachmentIds)).mapN { (oids, aids) =>
        val af = Statements.insertLinksAs(user, programId, oids, aids)
        session.prepareR(af.fragment.command).use { p =>
          p.execute(af.argument)
            .as(Result.unit)
            .recoverWith { case SqlState.ForeignKeyViolation(_) =>
              Result.failure(ForeignKeyViolationMessage(programId, aids)).pure[F]
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
        obsAttachmentIds: Nullable[List[ObsAttachment.Id]]
      )(using Transaction[F]): F[Result[Unit]] =
        obsAttachmentIds match {
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

    import ProgramUserService.Statements.{andWhereUserAccess, whereUserAccess}

    def insertLinksAs(
      user:             User,
      programId:        Program.Id,
      observationIds:   NonEmptyList[Observation.Id],
      obsAttachmentIds: NonEmptyList[ObsAttachment.Id]
    ): AppliedFragment =
      val insert: AppliedFragment =
        void"""
          INSERT INTO t_obs_attachment_assignment (
            c_program_id,
            c_observation_id,
            c_obs_attachment_id
          )
          SELECT * FROM (
            VALUES
        """

      val links: NonEmptyList[AppliedFragment] =
        for {
          oid <- observationIds
          aid <- obsAttachmentIds
        } yield sql"($program_id, $observation_id, $obs_attachment_id)" (programId, oid, aid)

      val values: AppliedFragment = links.intercalate(void", ")
      val as: AppliedFragment     =
        void""") AS t (c_program_id, c_observation_id, c_obs_attachment_id) """

      insert |+| values |+| as |+| whereUserAccess(user, programId) |+|
        void""" ON CONFLICT DO NOTHING""" // the key consists of all the columns, anyway

    private def programIdEqual(
      programId: Program.Id
    ): AppliedFragment =
      sql"c_program_id = $program_id" (programId)

    private def observationIdIn(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void", ") |+|
        void")"

    private def obsAttachmentIdIn(
      obsAttachmentIds: NonEmptyList[ObsAttachment.Id]
    ): AppliedFragment =
      void"c_obs_attachment_id IN (" |+|
        obsAttachmentIds.map(sql"$obs_attachment_id").intercalate(void", ") |+|
        void")"

    def deleteLinksAs(
      user:             User,
      programId:        Program.Id,
      observationIds:   NonEmptyList[Observation.Id],
      obsAttachmentIds: NonEmptyList[ObsAttachment.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_obs_attachment_assignment " |+|
        void"WHERE " |+| programIdEqual(programId) |+|
        void" AND " |+| observationIdIn(observationIds) |+|
        void" AND " |+| obsAttachmentIdIn(obsAttachmentIds) |+|
        andWhereUserAccess(user, programId)

    def deleteAllLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_obs_attachment_assignment " |+|
        void"WHERE " |+| programIdEqual(programId) |+|
        void" AND " |+| observationIdIn(observationIds) |+|
        andWhereUserAccess(user, programId)

    def clone(originalOid: Observation.Id, newOid: Observation.Id): AppliedFragment =
      sql"""
        INSERT INTO t_obs_attachment_assignment (
          c_program_id,
          c_observation_id,
          c_obs_attachment_id
        )
        SELECT 
          t_obs_attachment_assignment.c_program_id,
          $observation_id,
          t_obs_attachment_assignment.c_obs_attachment_id
        FROM t_obs_attachment_assignment
        JOIN t_obs_attachment ON t_obs_attachment.c_obs_attachment_id = t_obs_attachment_assignment.c_obs_attachment_id
        WHERE c_observation_id = $observation_id
      """.apply(newOid, originalOid)
  }
}
