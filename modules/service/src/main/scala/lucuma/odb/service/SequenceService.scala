// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.eq.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait SequenceService[F[_]] {

  def selectAtomRecord(
    atomId: Atom.Id
  )(using Transaction[F]): F[Option[SequenceService.AtomRecord]]

  def insertAtomRecord(
    instrument:   Instrument,
    visitId:      Visit.Id,
    stepCount:    NonNegShort,
    sequenceType: SequenceType
  )(using Transaction[F]): F[SequenceService.InsertAtomResponse]

  def insertGmosNorthStepRecord(
    atomId:     Atom.Id,
    instrument: GmosNorth,
    step:       StepConfig,
    stepIndex:  NonNegShort
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

  def insertGmosSouthStepRecord(
    atomId:     Atom.Id,
    instrument: GmosSouth,
    step:       StepConfig,
    stepIndex:  NonNegShort
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

}

object SequenceService {

  case class AtomRecord(
    atomId:        Atom.Id,
    observationId: Observation.Id,
    instrument:    Instrument,
    visitId:       Visit.Id,
    stepCount:     NonNegShort,
    sequenceType:  SequenceType,
    created:       Timestamp
  )

  sealed trait InsertAtomResponse extends Product with Serializable

  object InsertAtomResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertAtomResponse

    case class VisitNotFound(
      vid:        Visit.Id,
      instrument: Instrument
    ) extends InsertAtomResponse

    case class Success(
      aid: Atom.Id
    ) extends InsertAtomResponse

  }

  sealed trait InsertStepResponse extends Product with Serializable

  object InsertStepResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertStepResponse

    case class AtomNotFound(
      aid:        Atom.Id,
      instrument: Instrument
    ) extends InsertStepResponse

    case class Success(
      sid: Step.Id
    ) extends InsertStepResponse
  }

  def instantiate[F[_]: Concurrent: UUIDGen](using Services[F]): SequenceService[F] =
    new SequenceService[F] with ExecutionUserCheck {

      override def selectAtomRecord(
        atomId: Atom.Id
      )(using Transaction[F]): F[Option[AtomRecord]] =
        session.option(Statements.SelectAtom)(atomId)

      override def insertAtomRecord(
        instrument:   Instrument,
        visitId:      Visit.Id,
        stepCount:    NonNegShort,
        sequenceType: SequenceType
      )(using Transaction[F]): F[InsertAtomResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(InsertAtomResponse.NotAuthorized.apply))
          v    = visitService.select(visitId).map(_.filter(_.instrument === instrument))
          inv <- EitherT.fromOptionF(v, InsertAtomResponse.VisitNotFound(visitId, instrument))
          aid <- EitherT.right[InsertAtomResponse](UUIDGen[F].randomUUID.map(Atom.Id.fromUuid))
          _   <- EitherT.right[InsertAtomResponse](session.execute(Statements.InsertAtom)(aid, inv.observationId, instrument, visitId, stepCount, sequenceType))
        } yield InsertAtomResponse.Success(aid)).merge

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

      private def insertStepRecord(
        atomId:              Atom.Id,
        instrument:          Instrument,
        stepConfig:          StepConfig,
        stepIndex:           NonNegShort,
        insertDynamicConfig: Step.Id => F[Unit]
      )(using Transaction[F]): F[InsertStepResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          a    = selectAtomRecord(atomId).map(_.filter(_.instrument === instrument))
          inv <- EitherT.fromOptionF(a, AtomNotFound(atomId, instrument))
          sid <- EitherT.right[InsertStepResponse](UUIDGen[F].randomUUID.map(Step.Id.fromUuid))
          _   <- EitherT.right[InsertStepResponse](session.execute(Statements.InsertStep)(sid, instrument, atomId, stepConfig.stepType, stepIndex)).void
          _   <- EitherT.right(insertStepConfig(sid, stepConfig))
          _   <- EitherT.right(insertDynamicConfig(sid))
        } yield Success(sid)).merge

      override def insertGmosNorthStepRecord(
        atomId:        Atom.Id,
        dynamicConfig: GmosNorth,
        stepConfig:    StepConfig,
        stepIndex:     NonNegShort
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosNorth,
          stepConfig,
          stepIndex,
          sid => gmosSequenceService.insertGmosNorthDynamic(sid, dynamicConfig)
        )

      override def insertGmosSouthStepRecord(
        atomId:        Atom.Id,
        dynamicConfig: GmosSouth,
        stepConfig:    StepConfig,
        stepIndex:     NonNegShort
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosSouth,
          stepConfig,
          stepIndex,
          sid => gmosSequenceService.insertGmosSouthDynamic(sid, dynamicConfig)
        )

    }

  object Statements {

    private val atom_record: Codec[AtomRecord] =
      (
        atom_id        *:
        observation_id *:
        instrument     *:
        visit_id       *:
        int2_nonneg    *:
        sequence_type  *:
        core_timestamp
      ).to[AtomRecord]

    val SelectAtom: Query[Atom.Id, AtomRecord] =
      sql"""
        SELECT
          c_atom_id,
          c_observation_id,
          c_instrument,
          c_visit_id,
          c_step_count,
          c_sequence_type,
          c_created
        FROM t_atom_record
        WHERE c_atom_id = $atom_id
      """.query(atom_record)

    val InsertAtom: Command[(
      Atom.Id,
      Observation.Id,
      Instrument,
      Visit.Id,
      NonNegShort,
      SequenceType
    )] =
      sql"""
        INSERT INTO t_atom_record (
          c_atom_id,
          c_observation_id,
          c_instrument,
          c_visit_id,
          c_step_count,
          c_sequence_type
        ) SELECT
          $atom_id,
          $observation_id,
          $instrument,
          $visit_id,
          $int2_nonneg,
          $sequence_type
      """.command

    val InsertStep: Command[(
      Step.Id,
      Instrument,
      Atom.Id,
      StepType,
      NonNegShort
    )] =
      sql"""
        INSERT INTO t_step_record (
          c_step_id,
          c_instrument,
          c_atom_id,
          c_step_type,
          c_step_index
        ) SELECT
          $step_id,
          $instrument,
          $atom_id,
          $step_type,
          $int2_nonneg
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
