// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

sealed trait BlindOffsetsService[F[_]]:

  def createBlindOffset(
    programId: Program.Id,
    observationId: Observation.Id,
    input: TargetPropertiesInput.Create
  )(using Transaction[F], ServiceAccess): F[Result[Target.Id]]

  def removeBlindOffset(
    observationId: Observation.Id
  )(using Transaction[F], ServiceAccess): F[Unit]

  def getBlindOffset(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[Target.Id]]

  def updateBlindOffset(
    programId: Program.Id,
    observationId: Observation.Id,
    targetEnvironment: Option[TargetEnvironmentInput.Edit]
  )(using Transaction[F], ServiceAccess): F[Result[Unit]]

object BlindOffsetsService:

  def instantiate[F[_]: Concurrent](using Services[F]): BlindOffsetsService[F] =
    new BlindOffsetsService[F]:

      import Services.Syntax.*

      override def createBlindOffset(
        programId: Program.Id,
        observationId: Observation.Id,
        input: TargetPropertiesInput.Create
      )(using Transaction[F], ServiceAccess): F[Result[Target.Id]] =
        Services.asSuperUser:
          targetService.createTarget(
            AccessControl.unchecked(input, programId, program_id),
            TargetDisposition.BlindOffset
          ).flatMap: rTargetId =>
            rTargetId.traverse: targetId =>
              for {
                _ <- session.execute(Statements.addBlindOffset)(programId, observationId, targetId)
                _ <- session.execute(Statements.updateBlindOffsetFlag)(true, observationId)
              } yield targetId

      override def removeBlindOffset(
        observationId: Observation.Id
      )(using Transaction[F], ServiceAccess): F[Unit] =
        // This removes any existing blind offset target from the t_asterism_target table,
        // a database trigger permanently deletes the blind offset target from t_target
        session.execute(Statements.removeBlindOffset)(observationId).void

      override def getBlindOffset(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Target.Id]] =
        session.option(Statements.selectBlindOffset)(observationId)

      override def updateBlindOffset(
        programId: Program.Id,
        observationId: Observation.Id,
        targetEnvironment: Option[TargetEnvironmentInput.Edit]
      )(using Transaction[F], ServiceAccess): F[Result[Unit]] =
        targetEnvironment.fold(Result.unit.pure[F]): te =>
          te.blindOffsetTarget match
            case Nullable.Absent => Result.unit.pure[F] // No change
            case Nullable.Null =>
              removeBlindOffset(observationId).as(Result.unit)
            case NonNull(targetInput: TargetPropertiesInput.Create) =>
              for {
                // Remove existi
                _          <- removeBlindOffset(observationId)
                // Then create the new one
                result     <- createBlindOffset(programId, observationId, targetInput)
              } yield result.void

      private object Statements:

        val addBlindOffset: Command[Program.Id *: Observation.Id *: Target.Id *: EmptyTuple] =
          sql"""
            INSERT INTO t_asterism_target (c_program_id, c_observation_id, c_target_id)
            VALUES ($program_id, $observation_id, $target_id)
            ON CONFLICT (c_program_id, c_observation_id, c_target_id) DO NOTHING
          """.command

        val removeBlindOffset: Command[Observation.Id] =
          sql"""
            DELETE FROM t_asterism_target
            WHERE c_observation_id = $observation_id
            AND c_target_id IN (
              SELECT c_target_id FROM t_target
              WHERE c_target_disposition = 'blind_offset'
            )
          """.command

        val selectBlindOffset: Query[Observation.Id, Target.Id] =
          sql"""
            SELECT a.c_target_id
            FROM t_asterism_target a
            JOIN t_target t ON a.c_target_id = t.c_target_id
            WHERE a.c_observation_id = $observation_id
            AND t.c_target_disposition = 'blind_offset'
          """.query(target_id)

        val updateBlindOffsetFlag: Command[Boolean *: Observation.Id *: EmptyTuple] =
          sql"""
            UPDATE t_observation
            SET c_use_blind_offset = ${bool}
            WHERE c_observation_id = $observation_id
          """.command
