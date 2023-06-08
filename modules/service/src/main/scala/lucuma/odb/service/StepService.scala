// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.enums.GuideState
import lucuma.core.enums.Instrument
import lucuma.core.enums.StepType
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait StepService[F[_]] {

  def insertGmosNorth(
    visitId:    Visit.Id,
    instrument: GmosNorth,
    step:       StepConfig
  )(using Transaction[F]): F[StepService.InsertStepResponse]

  def insertGmosSouth(
    visitId:    Visit.Id,
    instrument: GmosSouth,
    step:       StepConfig
  )(using Transaction[F]): F[StepService.InsertStepResponse]

}

object StepService {

  sealed trait InsertStepResponse extends Product with Serializable

  object InsertStepResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertStepResponse

    case class VisitNotFound(
      vid:        Visit.Id,
      instrument: Instrument
    ) extends InsertStepResponse

    case class Success(
      sid: Step.Id
    ) extends InsertStepResponse
  }

  def instantiate[F[_]: Concurrent: UUIDGen](using Services[F]): StepService[F] =
    new StepService[F] with ExecutionUserCheck {

      import InsertStepResponse.*

      private def insertStepConfig(
        stepId:     Step.Id,
        stepConfig: StepConfig
      ): F[Unit] =
        stepConfig match {
          case StepConfig.Bias | StepConfig.Dark => Applicative[F].unit
          case s @ StepConfig.Gcal(_, _, _, _)   => session.execute(Statements.InsertStepConfigGcal)(stepId, s).void
          case s @ StepConfig.Science(_, _)      => session.execute(Statements.InsertStepConfigScience)(stepId, s).void
          case s @ StepConfig.SmartGcal(_)       => session.execute(Statements.InsertStepConfigSmartGcal)(stepId, s).void
        }

      private def insert(
        visitId:             Visit.Id,
        instrument:          Instrument,
        stepConfig:          StepConfig,
        insertDynamicConfig: Step.Id => F[Unit]
      )(using Transaction[F]): F[StepService.InsertStepResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          v    = visitService.select(visitId).map(_.filter(_.instrument === instrument))
          inv <- EitherT.fromOptionF(v, VisitNotFound(visitId, instrument))
          sid <- EitherT.right[InsertStepResponse](UUIDGen[F].randomUUID.map(Step.Id.fromUuid))
          _   <- EitherT.right[InsertStepResponse](session.execute(Statements.InsertStep)(sid, inv.observationId, instrument, visitId, stepConfig.stepType)).void
          _   <- EitherT.right(insertStepConfig(sid, stepConfig))
          _   <- EitherT.right(insertDynamicConfig(sid))
        } yield Success(sid)).merge


      override def insertGmosNorth(
        visitId:       Visit.Id,
        dynamicConfig: GmosNorth,
        stepConfig:    StepConfig
      )(using Transaction[F]): F[StepService.InsertStepResponse] =
        insert(
          visitId,
          Instrument.GmosNorth,
          stepConfig,
          sid => gmosSequenceService.insertGmosNorthDynamic(sid, dynamicConfig)
        )

      override def insertGmosSouth(
        visitId:       Visit.Id,
        dynamicConfig: GmosSouth,
        stepConfig:    StepConfig
      )(using Transaction[F]): F[StepService.InsertStepResponse] =
        insert(
          visitId,
          Instrument.GmosSouth,
          stepConfig,
          sid => gmosSequenceService.insertGmosSouthDynamic(sid, dynamicConfig)
        )

    }

  object Statements {

    val InsertStep: Command[(
      Step.Id,
      Observation.Id,
      Instrument,
      Visit.Id,
      StepType
    )] =
      sql"""
        INSERT INTO t_step (
          c_step_id,
          c_observation_id,
          c_visit_id,
          c_step_type
        ) SELECT
          $step_id,
          $observation_id,
          $instrument,
          $visit_id,
          $step_type
      """.command

    val InsertStepConfigGcal: Command[(Step.Id, StepConfig.Gcal)] =
      sql"""
        INSERT INTO t_step_config_gcal (
          c_step_id,
          c_gcal_continuum,
          c_gcal_ar_arc,
          c_gcal_cuar_arc,
          c_gcal_thar_arc,
          c_gcal_xe_arc,
          c_gcal_filter,
          c_gcal_diffuser,
          c_gcal_shutter
        ) SELECT
          $step_id,
          $step_config_gcal
      """.command

    val InsertStepConfigScience: Command[(Step.Id, StepConfig.Science)] =
      sql"""
        INSERT INTO t_step_config_science (
          c_step_id,
          c_offset_p,
          c_offset_q,
          c_guide_state
        ) SELECT
          $step_id,
          $step_config_science
      """.command

    val InsertStepConfigSmartGcal: Command[(Step.Id, StepConfig.SmartGcal)] =
      sql"""
        INSERT INTO t_step_config_smart_gcal (
          c_step_id,
          c_smart_gcal_type
        ) SELECT
          $step_id,
          $step_config_smart_gcal
      """.command

  }
}
