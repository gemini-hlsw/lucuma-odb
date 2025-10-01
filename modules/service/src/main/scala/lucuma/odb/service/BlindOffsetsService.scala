// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import scala.collection.immutable.SortedMap

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

  def createOrUpdateBlindOffsetFromCoordinates(
    programId: Program.Id,
    observationId: Observation.Id,
    coordinates: Coordinates
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
          targetService.createTarget(AccessControl.unchecked(input, programId, program_id))
            .flatMap: rTargetId =>
              rTargetId.traverse: targetId =>
                for {
                  _ <- session.execute(Statements.addBlindOffset)(programId, observationId, targetId, targetId)
                  _ <- session.execute(Statements.updateBlindOffsetFlag)(true, observationId)
                  _ <- session.execute(Statements.setManualOverride)(true, observationId)
                } yield targetId

      override def removeBlindOffset(
        observationId: Observation.Id
      )(using Transaction[F], ServiceAccess): F[Unit] =
        for {
          _ <- session.execute(Statements.removeBlindOffset)(observationId)
          _ <- session.execute(Statements.setManualOverride)(false, observationId)
        } yield ()

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
            case Nullable.Absent => Result.unit.pure[F]
            case Nullable.Null =>
              removeBlindOffset(observationId).as(Result.unit)
            case NonNull(targetInput: TargetPropertiesInput.Create) =>
              for {
                _          <- removeBlindOffset(observationId)
                result     <- createBlindOffset(programId, observationId, targetInput)
              } yield result.void

      override def createOrUpdateBlindOffsetFromCoordinates(
        programId: Program.Id,
        observationId: Observation.Id,
        coordinates: Coordinates
      )(using Transaction[F], ServiceAccess): F[Result[Unit]] =
        val targetInput = TargetPropertiesInput.Create(
          name = NonEmptyString.unsafeFrom("Blind Offset"),
          subtypeInfo = SiderealInput.Create(
            ra = coordinates.ra,
            dec = coordinates.dec,
            epoch = lucuma.core.math.Epoch.J2000,
            properMotion = None,
            radialVelocity = None,
            parallax = None,
            catalogInfo = None
          ),
          sourceProfile = SourceProfile.Point(
            SpectralDefinition.BandNormalized(
              Some(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G2V)),
              SortedMap.empty[Band, BrightnessMeasure[Integrated]]
            )
          ),
          existence = Existence.Present
        )
        for {
          _      <- removeBlindOffset(observationId)
          result <- createBlindOffset(programId, observationId, targetInput)
        } yield result.void

      private object Statements:

        val addBlindOffset: Command[Program.Id *: Observation.Id *: Target.Id *: Target.Id *: EmptyTuple] =
          sql"""
            WITH asterism_insert AS (
              INSERT INTO t_asterism_target (c_program_id, c_observation_id, c_target_id)
              VALUES ($program_id, $observation_id, $target_id)
              ON CONFLICT (c_program_id, c_observation_id, c_target_id) DO NOTHING
              RETURNING c_target_id
            )
            UPDATE t_target
            SET c_target_disposition = 'blind_offset'
            WHERE c_target_id = $target_id
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

        val setManualOverride: Command[Boolean *: Observation.Id *: EmptyTuple] =
          sql"""
            UPDATE t_obscalc
            SET c_blind_offset_manual_override = $bool
            WHERE c_observation_id = $observation_id
          """.command
