// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import edu.gemini.grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.target_id
import skunk.*
import skunk.implicits.*

trait AsterismService[F[_]] {

  def selectAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id]
  ): F[Map[Observation.Id, List[Target.Id]]]

  /**
   * Inserts (program, observation, target) triplets covering all combinations.
   * In other words, every observation in `observationIds` will be given all
   * the targets in `targetIds` in addition to any existing targets they may
   * already have.
   */
  def insertAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    targetIds:      NonEmptyList[Target.Id]
  ): F[Result[Unit]]

  /**
   * Deletes the asterisms associated with the given observation ids.
   */
  def deleteAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id]
  ): F[Result[Unit]]

  /**
   * Replaces the existing asterisms associated with the given observation ids
   * (if any) with the given targets.  This is essentially a delete followed
   * by an insert.
   */
  def setAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    targetIds:      Nullable[NonEmptyList[Target.Id]]
  ): F[Result[Unit]]

  /**
   * Updates the asterisms associated with each observation id, adding and
   * deleting targets as indicated.
   */
  def updateAsterism(
    programId:      Program.Id,
    observationIds: NonEmptyList[Observation.Id],
    ADD:            Option[NonEmptyList[Target.Id]],
    DELETE:         Option[NonEmptyList[Target.Id]]
  ): F[Result[Unit]]

}

object AsterismService {

  def ForeignKeyViolationMessage(
    programId: Program.Id,
    targetIds: NonEmptyList[Target.Id]
  ): String =
    s"Target(s) ${targetIds.map(_.show).intercalate(", ")} must exist and be associated with Program ${programId.show}."

  def fromSessionAndUser[F[_]: Sync](
    session: Session[F],
    user:    User
  ): AsterismService[F] =

    new AsterismService[F] {

      override def selectAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id]
      ): F[Map[Observation.Id, List[Target.Id]]] = {
        val (af, decoder) = Statements.selectLinksAs(user, programId, observationIds)
        session.prepare(af.fragment.query(decoder)).use { p =>
          p.stream(af.argument, 64)
           .compile
           .toList
           .map(_.groupMap(_._1)(_._2))
        }
      }

      override def insertAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      NonEmptyList[Target.Id]
      ): F[Result[Unit]] = {
        val af = Statements.insertLinksAs(user, programId, observationIds, targetIds)
        session.prepare(af.fragment.command).use { p =>
          p.execute(af.argument)
            .as(Result.unit)
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) =>
                Result.failure(ForeignKeyViolationMessage(programId, targetIds)).pure[F]
            }
        }
      }

      override def deleteAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id]
      ): F[Result[Unit]] =
        val af = Statements.deleteAllLinksAs(user, programId, observationIds)
        session.prepare(af.fragment.command).use { p =>
          p.execute(af.argument).as(Result.unit)
        }

      override def setAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        targetIds:      Nullable[NonEmptyList[Target.Id]]
      ): F[Result[Unit]] =
        targetIds match {
          case Nullable.Null          =>
            deleteAsterism(programId, observationIds)

          case Nullable.Absent        =>
            Result.unit.pure[F]

          case Nullable.NonNull(tids) =>
            deleteAsterism(programId, observationIds) *>
              insertAsterism(programId, observationIds, tids)
        }

      override def updateAsterism(
        programId:      Program.Id,
        observationIds: NonEmptyList[Observation.Id],
        ADD:            Option[NonEmptyList[Target.Id]],
        DELETE:         Option[NonEmptyList[Target.Id]]
      ): F[Result[Unit]] =
        ADD.fold(Result.unit.pure[F])(insertAsterism(programId, observationIds, _)) *>
          DELETE.fold(Result.unit.pure[F]) { tids =>
            val af = Statements.deleteLinksAs(user, programId, observationIds, tids)
            session.prepare(af.fragment.command).use { p =>
              p.execute(af.argument).as(Result.unit)
            }
          }

    }

  object Statements {

    import ProgramService.Statements.{existsUserAccess, whereUserAccess}

    def selectLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): (AppliedFragment, Decoder[(Observation.Id, Target.Id)]) =
      (void"""
        SELECT
          c_observation_id,
          c_target_id
        FROM
          t_asterism_target
        WHERE """ |+| programIdEqual(programId)               |+|
          void""" AND """ |+| observationIdIn(observationIds) |+|
          andExistsUserAccess(user, programId),
        observation_id ~ target_id
      )

    def insertLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id],
      targetIds:      NonEmptyList[Target.Id]
    ): AppliedFragment = {
      val insert: AppliedFragment =
        void"""
          INSERT INTO t_asterism_target (
            c_program_id,
            c_observation_id,
            c_target_id
          )
          SELECT * FROM (
            VALUES
        """
      val links: NonEmptyList[AppliedFragment] =
        for {
          oid <- observationIds
          tid <- targetIds
        } yield sql"($program_id, $observation_id, $target_id)"(programId ~ oid ~ tid)

      val values: AppliedFragment =
        links.intercalate(void", ")

      val as: AppliedFragment =
        void""") AS t (c_program_id, c_observation_id, c_target_id) """

      insert |+| values |+| as |+| whereUserAccess(user, programId) |+|
        void""" ON CONFLICT DO NOTHING"""  // the key consists of all the columns anyway
    }

    private def programIdEqual(
      programId: Program.Id
    ): AppliedFragment =
      sql"c_program_id = $program_id"(programId)

    private def observationIdIn(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"c_observation_id IN (" |+|
        observationIds.map(sql"$observation_id").intercalate(void", ") |+|
      void")"

    private def targetIdIn(
      targetIds: NonEmptyList[Target.Id]
    ): AppliedFragment =
      void"c_target_id IN (" |+|
        targetIds.map(sql"$target_id").intercalate(void", ") |+|
      void")"

    private def andExistsUserAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void" AND " |+| af
      }

    def deleteLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id],
      targetIds:      NonEmptyList[Target.Id]
    ): AppliedFragment =
       void"DELETE FROM ONLY t_asterism_target "        |+|
         void"WHERE " |+| programIdEqual(programId)     |+|
         void"AND " |+| observationIdIn(observationIds) |+|
         void"AND " |+| targetIdIn(targetIds)           |+|
         andExistsUserAccess(user, programId)

    def deleteAllLinksAs(
      user:           User,
      programId:      Program.Id,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"DELETE FROM ONLY t_asterism_target "         |+|
        void"WHERE " |+| programIdEqual(programId)      |+|
        void"AND "  |+| observationIdIn(observationIds) |+|
        andExistsUserAccess(user, programId)

  }

}
