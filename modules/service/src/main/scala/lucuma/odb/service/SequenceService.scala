// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Order.catsKernelOrderingForOrder
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Stream
import grackle.Result
import lucuma.core.enums.AtomStage
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.StepExecutionState
import lucuma.odb.sequence.TimeEstimateCalculator
import lucuma.odb.sequence.data.AtomRecord
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int2
import skunk.implicits.*

import Services.Syntax.*

trait SequenceService[F[_]]:

  def abandonOngoingSteps(
    observationId: Observation.Id
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def setAtomExecutionState(
    atomId: Atom.Id,
    stage:  AtomStage
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def abandonOngoingStepsExcept(
    observationId: Observation.Id,
    atomId:        Atom.Id,
    stepId:        Option[Step.Id]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def setStepExecutionState(
    stepId: Step.Id,
    stage:  StepStage,
    time:   Timestamp
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def insertAtomRecord(
    visitId:        Visit.Id,
    instrument:     Instrument,
    sequenceType:   SequenceType,
    generatedId:    Option[Atom.Id],
    idempotencyKey: Option[IdempotencyKey]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Atom.Id]]

  def insertFlamingos2StepRecord(
    atomId:         Atom.Id,
    instrument:     Flamingos2DynamicConfig,
    step:           StepConfig,
    telescope:      TelescopeConfig,
    observeClass:   ObserveClass,
    generatedId:    Option[Step.Id],
    idempotencyKey: Option[IdempotencyKey],
    timeCalculator: TimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]]

  def insertGmosNorthStepRecord(
    atomId:         Atom.Id,
    instrument:     GmosNorth,
    step:           StepConfig,
    telescope:      TelescopeConfig,
    observeClass:   ObserveClass,
    generatedId:    Option[Step.Id],
    idempotencyKey: Option[IdempotencyKey],
    timeCalculator: TimeEstimateCalculator[GmosNorthStatic, GmosNorth]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]]

  def insertGmosSouthStepRecord(
    atomId:         Atom.Id,
    instrument:     GmosSouth,
    step:           StepConfig,
    telescope:      TelescopeConfig,
    observeClass:   ObserveClass,
    generatedId:    Option[Step.Id],
    idempotencyKey: Option[IdempotencyKey],
    timeCalculator: TimeEstimateCalculator[GmosSouthStatic, GmosSouth]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]]

  def insertAtomDigests(
    observationId: Observation.Id,
    digests:       Stream[F, AtomDigest]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  def selectAtomDigests(
    which: List[Observation.Id]
  )(using Transaction[F], Services.ServiceAccess): Stream[F, (Observation.Id, Short, AtomDigest)]

  def selectAtomRecords(
    observationId: Observation.Id
  )(using Services.ServiceAccess): Stream[F, AtomRecord]

object SequenceService:

  sealed trait InsertAtomResponse extends Product with Serializable

  object InsertAtomResponse:
    case class VisitNotFound(
      vid:        Visit.Id,
      instrument: Instrument
    ) extends InsertAtomResponse

    case class Success(
      aid: Atom.Id
    ) extends InsertAtomResponse

  sealed trait InsertStepResponse extends Product with Serializable

  object InsertStepResponse:
    case class AtomNotFound(
      aid:        Atom.Id,
      instrument: Instrument
    ) extends InsertStepResponse

    case class Success(
      sid: Step.Id
    ) extends InsertStepResponse


  def instantiate[F[_]: Concurrent: UUIDGen](using Services[F]): SequenceService[F] =
    new SequenceService[F]:

      /**
       * We'll need to estimate the cost of executing the next step.  For that
       * we have to find the static config, the last gcal step (if any), the
       * last science step (if any) and the last step in general (if any).
       * This will serve as an input to the time estimate calculator so that it
       * can compare a new step being recorded with the previous state and
       * determine the cost of making the prescribed changes.
       */
      @annotation.nowarn("msg=unused implicit parameter")
      private def selectEstimatorState[S, D](
        observationId: Observation.Id,
        staticConfig:  Visit.Id => F[Option[S]],
        dynamicConfig: Step.Id => F[Option[D]]
      )(using Transaction[F]): F[Option[(S, TimeEstimateCalculator.Last[D])]] =
        for
          vid     <- session.option(Statements.SelectLastVisit)(observationId)
          static  <- vid.flatTraverse(staticConfig)
          gcal    <- session.option(Statements.SelectLastGcalConfig)(observationId)
          step    <- session.option(Statements.SelectLastStepConfig)(observationId)
          dynamic <- step.flatTraverse { case (id, _, _, _) => dynamicConfig(id) }
        yield static.tupleRight(
          TimeEstimateCalculator.Last(
            gcal,
            (step, dynamic).mapN { case ((_, stepConfig, telescopeConfig, observeClass), d) =>
              ProtoStep(d, stepConfig, telescopeConfig, observeClass)
            }
          )
        )

      private def selectFlamingos2EstimatorState(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[(Flamingos2StaticConfig, TimeEstimateCalculator.Last[Flamingos2DynamicConfig])]] =
        selectEstimatorState(
          observationId,
          services.flamingos2SequenceService.selectStatic,
          services.flamingos2SequenceService.selectDynamicForStep
        )

      private def selectGmosNorthEstimatorState(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[(GmosNorthStatic, TimeEstimateCalculator.Last[GmosNorth])]] =
        selectEstimatorState(
          observationId,
          services.gmosSequenceService.selectGmosNorthStatic,
          services.gmosSequenceService.selectGmosNorthDynamicForStep
        )

      private def selectGmosSouthEstimatorState(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[(GmosSouthStatic, TimeEstimateCalculator.Last[GmosSouth])]] =
        selectEstimatorState(
          observationId,
          services.gmosSequenceService.selectGmosSouthStatic,
          services.gmosSequenceService.selectGmosSouthDynamicForStep
        )

      override def setAtomExecutionState(
        atomId: Atom.Id,
        stage:  AtomStage
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val state = stage match
          case AtomStage.StartAtom => AtomExecutionState.Ongoing
          case AtomStage.EndAtom   => AtomExecutionState.Completed
        session.execute(Statements.SetAtomExecutionState)(state, atomId).void

      override def abandonOngoingSteps(
        observationId: Observation.Id
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        for
          _ <- session.execute(Statements.CompleteAllNonTerminalAtomsForObservation)(observationId)
          _ <- session.execute(Statements.AbandonAllNonTerminalStepsForObservation)(observationId)
        yield ()

      override def abandonOngoingStepsExcept(
        observationId: Observation.Id,
        atomId:        Atom.Id,
        stepId:        Option[Step.Id]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val abandonSteps =
          stepId.fold(session.execute(Statements.AbandonOngoingStepsWithoutAtomId)(observationId, atomId)): sid =>
            session.execute(Statements.AbandonOngoingStepsWithoutStepId)(observationId, sid)

        for
          _ <- session.execute(Statements.CompleteOngoingAtomsWithoutAtomId)(observationId, atomId)
          _ <- abandonSteps
        yield ()

      override def setStepExecutionState(
        stepId: Step.Id,
        stage:  StepStage,
        time:   Timestamp
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val state = stage match
          case StepStage.EndStep => StepExecutionState.Completed
          case StepStage.Abort   => StepExecutionState.Aborted
          case StepStage.Stop    => StepExecutionState.Stopped
          case _                 => StepExecutionState.Ongoing
        val completedTime = Option.when(stage === StepStage.EndStep)(time)
        session.execute(Statements.SetStepExecutionState)(state, completedTime, stepId).void

      @annotation.nowarn("msg=unused implicit parameter")
      def insertAtomRecordImpl(
        visitId:        Visit.Id,
        instrument:     Instrument,
        sequenceType:   SequenceType,
        generatedId:    Option[Atom.Id],
        idempotencyKey: Option[IdempotencyKey]
      )(using Transaction[F], Services.ServiceAccess): F[InsertAtomResponse] =
        val v = visitService.select(visitId).map(_.filter(_.instrument === instrument))
        (for
          inv  <- EitherT.fromOptionF(v, InsertAtomResponse.VisitNotFound(visitId, instrument))
          aid  <- EitherT.right[InsertAtomResponse](UUIDGen[F].randomUUID.map(Atom.Id.fromUuid))
          aidʹ <- EitherT.right[InsertAtomResponse](session.unique(Statements.InsertAtom)(aid, inv.observationId, visitId, instrument, sequenceType, generatedId, idempotencyKey))
        yield InsertAtomResponse.Success(aidʹ)).merge

      override def insertAtomRecord(
        visitId:        Visit.Id,
        instrument:     Instrument,
        sequenceType:   SequenceType,
        generatedId:    Option[Atom.Id],
        idempotencyKey: Option[IdempotencyKey]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Atom.Id]] =
        insertAtomRecordImpl(visitId, instrument, sequenceType, generatedId, idempotencyKey).map:
          case InsertAtomResponse.VisitNotFound(id, instrument) => OdbError.InvalidVisit(id, Some(s"Visit '$id' not found or is not a ${instrument.longName} visit")).asFailure
          case InsertAtomResponse.Success(aid)                  => Result.success(aid)

      import InsertStepResponse.*

      private def insertStepConfig(
        stepId:     Step.Id,
        stepConfig: StepConfig
      ): F[Unit] =
        stepConfig match {
          case StepConfig.Bias | StepConfig.Dark | StepConfig.Science => Applicative[F].unit
          case s @ StepConfig.Gcal(_, _, _, _) => session.execute(Statements.InsertStepConfigGcal)(stepId, s).void
          case s @ StepConfig.SmartGcal(_)     => session.execute(Statements.InsertStepConfigSmartGcal)(stepId, s).void
        }

      @annotation.nowarn("msg=unused implicit parameter")
      private def insertStepRecord[S, D](
        atomId:              Atom.Id,
        instrument:          Instrument,
        stepConfig:          StepConfig,
        telescopeConfig:     TelescopeConfig,
        observeClass:        ObserveClass,
        generatedId:         Option[Step.Id],
        idempotencyKey:      Option[IdempotencyKey],
        timeEstimate:        (S, TimeEstimateCalculator.Last[D]) => StepEstimate,
        estimatorState:      Observation.Id => F[Option[(S, TimeEstimateCalculator.Last[D])]],
        insertDynamicConfig: Step.Id => F[Unit]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]] =
        val foo = session.option(Statements.SelectObservationId)((atomId, instrument))
        val fos = OptionT(foo).flatMap(o => OptionT(estimatorState(o))).value
        (for {
          sid  <- EitherT.right(UUIDGen[F].randomUUID.map(Step.Id.fromUuid))
          es   <- EitherT.fromOptionF(fos, AtomNotFound(atomId, instrument))
          sidʹ <- EitherT.right(session.unique(Statements.InsertStep)(
                    sid, atomId, instrument, stepConfig.stepType, telescopeConfig, observeClass, generatedId, idempotencyKey, timeEstimate.tupled(es).total
                  ))
                  // If the insertion uses the given random step id, it must be new.  Otherwise, we are
                  // returning an existing one.
          _    <- if sid === sidʹ then EitherT.right(insertStepConfig(sidʹ, stepConfig)) else EitherT.pure(())
          _    <- if sid === sidʹ then EitherT.right(insertDynamicConfig(sidʹ)) else EitherT.pure(())
        } yield Success(sidʹ)).merge.map:
          case AtomNotFound(id, instrument)  => OdbError.InvalidAtom(id, Some(s"Atom '$id' not found or is not a ${instrument.longName} atom")).asFailure
          case Success(sid)                  => Result(sid)

      def insertFlamingos2StepRecord(
        atomId:          Atom.Id,
        dynamicConfig:   Flamingos2DynamicConfig,
        stepConfig:      StepConfig,
        telescopeConfig: TelescopeConfig,
        observeClass:    ObserveClass,
        generatedId:     Option[Step.Id],
        idempotencyKey:  Option[IdempotencyKey],
        timeCalculator:  TimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]] =
        insertStepRecord(
          atomId,
          Instrument.Flamingos2,
          stepConfig,
          telescopeConfig,
          observeClass,
          generatedId,
          idempotencyKey,
          timeCalculator.estimateStep(_, _, ProtoStep(dynamicConfig, stepConfig, telescopeConfig, observeClass)),
          selectFlamingos2EstimatorState,
          sid => flamingos2SequenceService.insertDynamic(sid, dynamicConfig)
        )

      override def insertGmosNorthStepRecord(
        atomId:          Atom.Id,
        dynamicConfig:   GmosNorth,
        stepConfig:      StepConfig,
        telescopeConfig: TelescopeConfig,
        observeClass:    ObserveClass,
        generatedId:     Option[Step.Id],
        idempotencyKey:  Option[IdempotencyKey],
        timeCalculator:  TimeEstimateCalculator[GmosNorthStatic, GmosNorth]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]] =
        insertStepRecord(
          atomId,
          Instrument.GmosNorth,
          stepConfig,
          telescopeConfig,
          observeClass,
          generatedId,
          idempotencyKey,
          timeCalculator.estimateStep(_, _, ProtoStep(dynamicConfig, stepConfig, telescopeConfig, observeClass)),
          selectGmosNorthEstimatorState,
          sid => gmosSequenceService.insertGmosNorthDynamic(sid, dynamicConfig)
        )

      override def insertGmosSouthStepRecord(
        atomId:          Atom.Id,
        dynamicConfig:   GmosSouth,
        stepConfig:      StepConfig,
        telescopeConfig: TelescopeConfig,
        observeClass:    ObserveClass,
        generatedId:     Option[Step.Id],
        idempotencyKey:  Option[IdempotencyKey],
        timeCalculator:  TimeEstimateCalculator[GmosSouthStatic, GmosSouth]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Step.Id]] =
        insertStepRecord(
          atomId,
          Instrument.GmosSouth,
          stepConfig,
          telescopeConfig,
          observeClass,
          generatedId,
          idempotencyKey,
          timeCalculator.estimateStep(_, _, ProtoStep(dynamicConfig, stepConfig, telescopeConfig, observeClass)),
          selectGmosSouthEstimatorState,
          sid => gmosSequenceService.insertGmosSouthDynamic(sid, dynamicConfig)
        )

      override def insertAtomDigests(
        observationId: Observation.Id,
        digests:       Stream[F, AtomDigest]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        val insert =
          digests
            .zipWithIndex
            .map: (d, i) =>
              (observationId, i.toShort, d)
            .chunkN(256)
            .evalTap: c =>
              val lst = c.toList
              session.execute(Statements.insertAtomDigest(lst))(lst)

        for
          _ <- session.execute(Statements.DeleteAtomDigests)(observationId)
          _ <- insert.compile.drain
        yield ()

      override def selectAtomDigests(
        which: List[Observation.Id]
      )(using Transaction[F], Services.ServiceAccess): Stream[F, (Observation.Id, Short, AtomDigest)] =
        if which.isEmpty then Stream.empty
        else session.stream(Statements.selectAtomDigests(which))(which, 1024)

      override def selectAtomRecords(
        observationId: Observation.Id
      )(using Services.ServiceAccess): Stream[F, AtomRecord] =
        session.stream(Statements.SelectAtomRecords)(observationId, 1024)

  object Statements:

    val SelectObservationId: Query[(Atom.Id, Instrument), Observation.Id] =
      sql"""
        SELECT c_observation_id
          FROM t_atom_record
         WHERE c_atom_id = $atom_id AND c_instrument = $instrument
      """.query(observation_id)

    val InsertAtom: Query[(
      Atom.Id,
      Observation.Id,
      Visit.Id,
      Instrument,
      SequenceType,
      Option[Atom.Id],
      Option[IdempotencyKey]
    ), Atom.Id] =
      sql"""
        INSERT INTO t_atom_record (
          c_atom_id,
          c_observation_id,
          c_visit_id,
          c_instrument,
          c_sequence_type,
          c_generated_id,
          c_idempotency_key
        ) SELECT
          $atom_id,
          $observation_id,
          $visit_id,
          $instrument,
          $sequence_type,
          ${atom_id.opt},
          ${idempotency_key.opt}
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_atom_id
      """.query(atom_id)

    val InsertStep: Query[(
      Step.Id,
      Atom.Id,
      Instrument,
      StepType,
      TelescopeConfig,
      ObserveClass,
      Option[Step.Id],
      Option[IdempotencyKey],
      TimeSpan,
    ), Step.Id] =
      sql"""
        INSERT INTO t_step_record (
          c_step_id,
          c_step_index,
          c_atom_id,
          c_instrument,
          c_step_type,
          c_offset_p,
          c_offset_q,
          c_guide_state,
          c_observe_class,
          c_generated_id,
          c_idempotency_key,
          c_time_estimate
        ) SELECT
          $step_id,
          COALESCE(
            (SELECT MAX(c_step_index) + 1
             FROM t_step_record AS s
             INNER JOIN t_atom_record AS a ON a.c_atom_id = s.c_atom_id
             WHERE a.c_observation_id = (SELECT c_observation_id FROM t_atom_record WHERE c_atom_id = $atom_id)
            ),
            1
          ),
          $atom_id,
          $instrument,
          $step_type,
          $telescope_config,
          $obs_class,
          ${step_id.opt},
          ${idempotency_key.opt},
          $time_span
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_step_id
      """.query(step_id).contramap { (s, a, i, st, tc, oc, g, idm, d) => (s, a, a, i, st, tc, oc, g, idm, d) }

    private def insertStepConfigFragment(table: String, columns: List[String]): Fragment[Void] =
      sql"""
        INSERT INTO #$table (
          c_step_id,
          #${encodeColumns(none, columns)}
        )
      """

    private val StepConfigGcalColumns: List[String] =
      List(
        "c_gcal_continuum",
        "c_gcal_ar_arc",
        "c_gcal_cuar_arc",
        "c_gcal_thar_arc",
        "c_gcal_xe_arc",
        "c_gcal_filter",
        "c_gcal_diffuser",
        "c_gcal_shutter"
      )

    val InsertStepConfigGcal: Command[(Step.Id, StepConfig.Gcal)] =
      sql"""
        ${insertStepConfigFragment("t_step_config_gcal", StepConfigGcalColumns)} SELECT
          $step_id,
          $step_config_gcal
      """.command

    private val StepConfigSmartGcalColumns: List[String] =
      List(
        "c_smart_gcal_type"
      )

    val InsertStepConfigSmartGcal: Command[(Step.Id, StepConfig.SmartGcal)] =
      sql"""
        ${insertStepConfigFragment("t_step_config_smart_gcal", StepConfigSmartGcalColumns)} SELECT
          $step_id,
          $step_config_smart_gcal
      """.command

    private val step_config: Codec[StepConfig] =
      (
        step_type               *:
        step_config_gcal.opt    *:
        step_config_smart_gcal.opt
      ).eimap { case (stepType, oGcal, oSmart) =>
        stepType match {
          case StepType.Bias      => StepConfig.Bias.asRight
          case StepType.Dark      => StepConfig.Dark.asRight
          case StepType.Gcal      => oGcal.toRight("Missing gcal step config definition")
          case StepType.Science   => StepConfig.Science.asRight
          case StepType.SmartGcal => oSmart.toRight("Missing smart gcal step config definition")
        }
      } { stepConfig =>
        (stepConfig.stepType,
         StepConfig.gcal.getOption(stepConfig),
         StepConfig.smartGcal.getOption(stepConfig)
        )
      }

    private val timestamp_interval: Codec[TimestampInterval] =
      (core_timestamp *: core_timestamp)
        .imap((first, last) => TimestampInterval.between(first, last))(interval =>
          (interval.start, interval.end)
        )

    val SelectStepConfigForObs: Query[Observation.Id, (Step.Id, StepConfig)] =
      (sql"""
        SELECT
          v.c_step_id,
          v.c_step_type,
          #${encodeColumns("v".some, StepConfigGcalColumns)},
          #${encodeColumns("v".some, StepConfigSmartGcalColumns)}
        FROM v_step_record v
        INNER JOIN t_atom_record a ON a.c_atom_id = v.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id"""
      ).query(step_id *: step_config)



    private def step_record[D](dynamic_config: Decoder[D]): Decoder[StepRecord[D]] =
      (
        step_id                *:
        atom_id                *:
        visit_id               *:
        int4_pos               *:
        step_config            *:
        telescope_config       *:
        instrument             *:
        dynamic_config         *:
        core_timestamp         *:
        timestamp_interval.opt *:
        sequence_type          *:
        obs_class              *:
        step_execution_state   *:
        dataset_qa_state.opt
      ).to[StepRecord[D]]

    def selectStepRecord[D](
      instTable:   String,
      instAlias:   String,
      instColumns: List[String],
      instDecoder: Decoder[D]
    ): Query[Observation.Id, StepRecord[D]] =
      (sql"""
        SELECT
          v.c_step_id,
          v.c_atom_id,
          v.c_visit_id,
          v.c_step_index,
          v.c_step_type,
          #${encodeColumns("v".some, StepConfigGcalColumns)},
          #${encodeColumns("v".some, StepConfigSmartGcalColumns)},
          v.c_offset_p,
          v.c_offset_q,
          v.c_guide_state,
          v.c_instrument,
          #${encodeColumns(instAlias.some, instColumns)},
          v.c_created,
          v.c_first_event_time,
          v.c_last_event_time,
          a.c_sequence_type,
          v.c_observe_class,
          v.c_execution_state,
          v.c_qa_state
        FROM v_step_record v
        INNER JOIN #$instTable #$instAlias ON #$instAlias.c_step_id = v.c_step_id
        INNER JOIN t_atom_record a ON a.c_atom_id = v.c_atom_id
        WHERE a.c_observation_id = $observation_id
        ORDER BY v.c_created
      """).query(step_record(instDecoder))

    val SetStepExecutionState: Command[(StepExecutionState, Option[Timestamp], Step.Id)] =
      sql"""
        UPDATE t_step_record s
           SET c_execution_state = $step_execution_state,
               c_completed       = ${core_timestamp.opt}
          FROM t_step_execution_state e
         WHERE s.c_execution_state = e.c_tag
           AND e.c_terminal = FALSE
           AND s.c_step_id = $step_id
      """.command

    val AbandonAllNonTerminalStepsForObservation: Command[Observation.Id] =
      sql"""
        UPDATE t_step_record s
           SET c_execution_state = 'abandoned'
          FROM t_atom_record a, t_step_execution_state e
         WHERE s.c_atom_id = a.c_atom_id
           AND s.c_execution_state = e.c_tag
           AND a.c_observation_id = $observation_id
           AND e.c_terminal = FALSE
      """.command

    val AbandonOngoingStepsWithoutStepId: Command[(Observation.Id, Step.Id)] =
      sql"""
        UPDATE t_step_record s
           SET c_execution_state = 'abandoned'
          FROM t_atom_record a
         WHERE s.c_atom_id = a.c_atom_id
           AND a.c_observation_id = $observation_id
           AND s.c_step_id != $step_id
           AND s.c_execution_state = 'ongoing';
      """.command

    val AbandonOngoingStepsWithoutAtomId: Command[(Observation.Id, Atom.Id)] =
      sql"""
        UPDATE t_step_record s
           SET c_execution_state = 'abandoned'
          FROM t_atom_record a
         WHERE s.c_atom_id = a.c_atom_id
           AND a.c_observation_id = $observation_id
           AND a.c_atom_id != $atom_id
           AND s.c_execution_state = 'ongoing';
      """.command

    val SetAtomExecutionState: Command[(AtomExecutionState, Atom.Id)] =
      sql"""
        UPDATE t_atom_record a
           SET c_execution_state = $atom_execution_state
          FROM t_atom_execution_state e
         WHERE a.c_execution_state = e.c_tag
           AND a.c_atom_id = $atom_id
      """.command

    val CompleteAllNonTerminalAtomsForObservation: Command[Observation.Id] =
      sql"""
        UPDATE t_atom_record a
           SET c_execution_state = 'completed'
          FROM t_atom_execution_state e
         WHERE a.c_execution_state = e.c_tag
           AND a.c_observation_id = $observation_id
           AND e.c_terminal = FALSE
      """.command

    val CompleteOngoingAtomsWithoutAtomId: Command[(Observation.Id, Atom.Id)] =
      sql"""
        UPDATE t_atom_record
           SET c_execution_state = 'completed'
         WHERE c_observation_id = $observation_id
           AND c_atom_id != $atom_id
           AND c_execution_state = 'ongoing';
      """.command

    val SelectLastVisit: Query[Observation.Id, Visit.Id] =
      sql"""
        SELECT c_visit_id
          FROM t_visit
         WHERE c_observation_id = $observation_id
      ORDER BY c_created DESC
         LIMIT 1
      """.query(visit_id)

    val SelectLastGcalConfig: Query[Observation.Id, StepConfig.Gcal] =
      sql"""
        SELECT
          #${encodeColumns("s".some, StepConfigGcalColumns)}
        FROM v_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          a.c_observation_id = $observation_id AND
          s.c_step_type      = 'gcal'
        ORDER BY s.c_created DESC
        LIMIT 1
      """.query(step_config_gcal)

    val SelectLastStepConfig: Query[Observation.Id, (Step.Id, StepConfig, TelescopeConfig, ObserveClass)] =
      sql"""
        SELECT
          s.c_step_id,
          s.c_step_type,
          #${encodeColumns("s".some, StepConfigGcalColumns)},
          #${encodeColumns("s".some, StepConfigSmartGcalColumns)},
          s.c_offset_p,
          s.c_offset_q,
          s.c_guide_state,
          s.c_observe_class
        FROM v_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          a.c_observation_id = $observation_id
        ORDER BY s.c_created DESC
        LIMIT 1
      """.query(step_id *: step_config *: telescope_config *: obs_class)

    val DeleteAtomDigests: Command[Observation.Id] =
      sql"""
        DELETE FROM t_atom_digest WHERE c_observation_id = $observation_id
      """.command

    val atom_digest: Codec[AtomDigest] = (
      atom_id        *:
      obs_class      *:
      time_span      *:
      time_span      *:
      _step_type     *:
      _gcal_lamp_type
    ).imap { case (a, c, n, p, ss, ls) =>
      AtomDigest(
        a,
        c,
        CategorizedTime(ChargeClass.NonCharged -> n, ChargeClass.Program -> p),
        ss.toSet,
        ls.toSet
      )
    } { (a: AtomDigest) => (
      a.id,
      a.observeClass,
      a.timeEstimate.nonCharged,
      a.timeEstimate.programTime,
      a.stepTypes.toList.sorted,
      a.lampTypes.toList.sorted
    )}

    val atom_digest_row: Codec[(Observation.Id, Short, AtomDigest)] =
      observation_id *: int2 *: atom_digest

    val atom_record: Codec[AtomRecord] = (
      atom_id              *:
      visit_id             *:
      sequence_type        *:
      core_timestamp       *:
      atom_execution_state *:
      atom_id.opt
    ).to[AtomRecord]

    val AtomDigestRowColumns: String =
      """
          c_observation_id,
          c_atom_index,
          c_atom_id,
          c_observe_class,
          c_non_charged_time_estimate,
          c_program_time_estimate,
          c_step_types,
          c_lamp_types
      """

    def insertAtomDigest(ds: List[(Observation.Id, Short, AtomDigest)]): Command[ds.type] =
      val enc = atom_digest_row.values.list(ds)
      sql"""
        INSERT INTO t_atom_digest (
          #$AtomDigestRowColumns
        ) VALUES $enc
      """.command

    def selectAtomDigests(which: List[Observation.Id]): Query[which.type, (Observation.Id, Short, AtomDigest)] =
      sql"""
        SELECT
          #$AtomDigestRowColumns
        FROM
          t_atom_digest
        WHERE
          c_observation_id IN ${observation_id.list(which).values}
        ORDER BY c_observation_id, c_atom_index
      """.query(atom_digest_row)

    val SelectAtomRecords: Query[Observation.Id, AtomRecord] =
      sql"""
        SELECT
          c_atom_id,
          c_visit_id,
          c_sequence_type,
          c_created,
          c_execution_state,
          c_generated_id
        FROM
          t_atom_record
        WHERE
          c_observation_id = $observation_id
        ORDER BY c_observation_id, c_created
      """.query(atom_record)