// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.show.*
import edu.gemini.grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.target_id
import skunk.*
import skunk.implicits.*

trait AsterismService[F[_]] {

  def insertAsterism(
    programId:     Program.Id,
    observationId: Observation.Id,
    targetIds:     NonEmptyList[Target.Id]
  ): F[Result[Unit]]

}

object AsterismService {

  def ForeignKeyViolationMessage(
    programId: Program.Id,
    targetIds: NonEmptyList[Target.Id]
  ): String =
    s"Target(s) ${targetIds.map(_.show).intercalate(", ")} must exist and be associated with Program ${programId.show}."


  /*
  create table t_asterism_target (

    c_program_id     d_program_id     not null,
    c_observation_id d_observation_id not null,
    c_target_id      d_target_id      not null,

    foreign key (c_program_id, c_observation_id) references t_observation(c_program_id, c_observation_id),
    foreign key (c_program_id, c_target_id)      references t_target(c_program_id, c_target_id),
    constraint t_asterism_target_pkey primary key (c_program_id, c_observation_id, c_target_id)

  )
  */

  def fromSessionAndUser[F[_]: Sync](
    session: Session[F],
    user:    User
  ): AsterismService[F] =

    new AsterismService[F] {
      override def insertAsterism(
        programId:     Program.Id,
        observationId: Observation.Id,
        targetIds:     NonEmptyList[Target.Id]
      ): F[Result[Unit]] = {
        val af = Statements.insertLinksAs(user, programId, observationId, targetIds.toList)
        session.prepare(af.fragment.command).use { p =>
          p.execute(af.argument)
            .as(Result.unit)
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) =>
                Result.failure(ForeignKeyViolationMessage(programId, targetIds)).pure[F]
            }
        }
      }
    }

  object Statements {

    import ProgramService.Statements.whereUserAccess

    def insertLinksAs(
      user:          User,
      programId:     Program.Id,
      observationId: Observation.Id,
      targetIds:     List[Target.Id]
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

      val values: AppliedFragment =
        targetIds.map { t =>
          sql"""($program_id, $observation_id, $target_id)"""
            .apply(programId ~ observationId ~ t)
        }.intercalate(void", ")

      val as: AppliedFragment =
        void""") AS t (c_program_id, c_observation_id, c_target_id)"""

      insert |+| values |+| as |+| whereUserAccess(user, programId)
    }

  }

}
