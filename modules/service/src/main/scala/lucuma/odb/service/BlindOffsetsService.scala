// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.graphql.input.CloneTargetInput
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
    input: TargetPropertiesInput.Create,
    blindOffsetType: BlindOffsetType = BlindOffsetType.Manual
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

  def cloneBlindOffset(
    programId: Program.Id,
    oldObservationId: Observation.Id,
    newObservationId: Observation.Id
  )(using Transaction[F]): F[Result[Unit]]

object BlindOffsetsService:

  def instantiate[F[_]: Concurrent](using Services[F]): BlindOffsetsService[F] =
    new BlindOffsetsService[F]:

      import Services.Syntax.*

      override def createBlindOffset(
        programId: Program.Id,
        observationId: Observation.Id,
        input: TargetPropertiesInput.Create,
        blindOffsetType: BlindOffsetType = BlindOffsetType.Manual
      )(using Transaction[F], ServiceAccess): F[Result[Target.Id]] =
        Services.asSuperUser:
          targetService.createTarget(
            AccessControl.unchecked(input, programId, program_id),
            TargetDisposition.BlindOffset
          ).flatMap: rTargetId =>
            rTargetId.traverse: targetId =>
              for {
                _ <- session.execute(Statements.updateBlindOffsetFields)(targetId, true, blindOffsetType, observationId)
              } yield targetId

      override def removeBlindOffset(
        observationId: Observation.Id
      )(using Transaction[F], ServiceAccess): F[Unit] =
        // This permanently deletes any existing blind offset target from the t_target table,
        // database triggers set c_blind_offset_target_id to null and c_blind_offset_type to MANUAL
        session.execute(Statements.removeBlindOffset)(observationId).void

      override def getBlindOffset(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Target.Id]] =
        session.option(Statements.selectBlindOffset)(observationId).map(_.flatten)

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
                // Then create the new one.
                result     <- createBlindOffset(programId, observationId, targetInput, te.blindOffsetType)
              } yield result.void

      override def cloneBlindOffset(
        programId: Program.Id,
        oldObservationId: Observation.Id,
        newObservationId: Observation.Id
      )(using Transaction[F]): F[Result[Unit]] = 
        getBlindOffset(oldObservationId).flatMap:
          _.fold(Result.unit.pure): tid =>
            val input = CloneTargetInput(tid, SET = None, REPLACE_IN = NonEmptyList.one(newObservationId).some)
            Services.asSuperUser:
              targetService.cloneTarget(
                AccessControl.unchecked(input, programId, program_id)
              ).flatMap:
                _.traverse((_, tid) =>
                  session.execute(Statements.updateBlindOffsetTargetId)(tid, newObservationId).void
                )

      private object Statements:

        val removeBlindOffset: Command[Observation.Id] =
          sql"""
            DELETE FROM t_target t
            USING t_observation o
            WHERE o.c_observation_id = $observation_id
              AND t.c_target_id = o.c_blind_offset_target_id
          """.command

        val selectBlindOffset: Query[Observation.Id, Option[Target.Id]] =
          sql"""
            SELECT c_blind_offset_target_id
            FROM t_observation
            WHERE c_observation_id = $observation_id
          """.query(target_id.opt)

        val updateBlindOffsetFields: Command[(Target.Id, Boolean, BlindOffsetType, Observation.Id)] =
          sql"""
            UPDATE t_observation
            SET c_blind_offset_target_id = $target_id,
                c_use_blind_offset = ${bool},
                c_blind_offset_type = ${blind_offset_type}
            WHERE c_observation_id = $observation_id
          """.command

        val updateBlindOffsetTargetId: Command[(Target.Id, Observation.Id)] =
          sql"""
            UPDATE t_observation
            SET c_blind_offset_target_id = $target_id
            WHERE c_observation_id = $observation_id
          """.command
            
