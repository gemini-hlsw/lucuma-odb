// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.StaticConfig.{GmosNorth => GmosNorthStatic}
import lucuma.core.model.sequence.gmos.StaticConfig.{GmosSouth => GmosSouthStatic}
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.logic.EstimatorState
import lucuma.odb.logic.TimeEstimateCalculator
import lucuma.odb.sequence.data.CompletedAtomMap
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait SequenceService[F[_]] {

  def selectGmosNorthCompletedAtomMap(
    observationId: Observation.Id
  )(using Transaction[F]): F[CompletedAtomMap[GmosNorth]]

  def selectGmosNorthSteps(
    observationId: Observation.Id
  )(using Transaction[F]): F[Map[Step.Id, (GmosNorth, StepConfig)]]

  def selectGmosSouthCompletedAtomMap(
    observationId: Observation.Id
  )(using Transaction[F]): F[CompletedAtomMap[GmosSouth]]

  def selectGmosSouthSteps(
    observationId: Observation.Id
  )(using Transaction[F]): F[Map[Step.Id, (GmosSouth, StepConfig)]]

  def setStepCompleted(
    stepId: Step.Id,
    time:   Option[Timestamp]
  )(using Transaction[F]): F[Unit]

  def insertAtomRecord(
    visitId:      Visit.Id,
    instrument:   Instrument,
    stepCount:    NonNegShort,
    sequenceType: SequenceType
  )(using Transaction[F]): F[SequenceService.InsertAtomResponse]

  def insertGmosNorthStepRecord(
    atomId:         Atom.Id,
    instrument:     GmosNorth,
    step:           StepConfig,
    observeClass:   ObserveClass,
    timeCalculator: TimeEstimateCalculator[GmosNorthStatic, GmosNorth]
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

  def insertGmosSouthStepRecord(
    atomId:         Atom.Id,
    instrument:     GmosSouth,
    step:           StepConfig,
    observeClass:   ObserveClass,
    timeCalculator: TimeEstimateCalculator[GmosSouthStatic, GmosSouth]
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

}

object SequenceService {

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

      override def selectGmosNorthCompletedAtomMap(
        observationId: Observation.Id
      )(using Transaction[F]): F[CompletedAtomMap[GmosNorth]] =
        selectCompletedAtomMap(
          observationId,
          gmosSequenceService.selectGmosNorthDynamic(observationId)
        )

      override def selectGmosSouthCompletedAtomMap(
        observationId: Observation.Id
      )(using Transaction[F]): F[CompletedAtomMap[GmosSouth]] =
        selectCompletedAtomMap(
          observationId,
          gmosSequenceService.selectGmosSouthDynamic(observationId)
        )

      private def selectCompletedAtomMap[D](
        observationId:  Observation.Id,
        dynamicConfigs: Stream[F, (Step.Id, D)]
      )(using Transaction[F]): F[CompletedAtomMap[D]] =
        stepRecordMap(observationId, dynamicConfigs)
          .flatMap(completedAtomMap(observationId))

      def selectGmosNorthSteps(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[Step.Id, (GmosNorth, StepConfig)]] =
        stepRecordMap(observationId, gmosSequenceService.selectGmosNorthDynamic(observationId))

      def selectGmosSouthSteps(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[Step.Id, (GmosSouth, StepConfig)]] =
        stepRecordMap(observationId, gmosSequenceService.selectGmosSouthDynamic(observationId))

      /**
       * We'll need to estimate the cost of executing the next step.  For that
       * we have to find the static config, the last gcal step (if any), the
       * last science step (if any) and the last step in general (if any).
       * This will serve as an input to the time estimate calculator so that it
       * can compare a new step being recorded with the previous state and
       * determine the cost of making the prescribed changes.
       */
      private def selectEstimatorState[S, D](
        observationId: Observation.Id,
        staticConfig:  Visit.Id => F[Option[S]],
        dynamicConfig: Step.Id => F[Option[D]]
      )(using Transaction[F]): F[Option[(S, EstimatorState[D])]] =
        for {
          vid     <- session.option(Statements.SelectLastVisit)(observationId)
          static  <- vid.flatTraverse(staticConfig)
          gcal    <- session.option(Statements.SelectLastGcalConfig)(observationId)
          science <- session.option(Statements.SelectLastScienceConfig)(observationId)
          step    <- session.option(Statements.SelectLastStepConfig)(observationId)
          dynamic <- step.flatTraverse { case (id, _, _) => dynamicConfig(id) }
        } yield static.tupleRight(
          EstimatorState(
            gcal,
            science,
            (step, dynamic).mapN { case ((_, stepConfig, observeClass), d) =>
              ProtoStep(d, stepConfig, observeClass)
            }
          )
        )

      private def selectGmosNorthEstimatorState(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[(GmosNorthStatic, EstimatorState[GmosNorth])]] =
        selectEstimatorState(
          observationId,
          services.gmosSequenceService.selectGmosNorthStatic,
          services.gmosSequenceService.selectGmosNorthDynamicStep
        )

      private def selectGmosSouthEstimatorState(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[(GmosSouthStatic, EstimatorState[GmosSouth])]] =
        selectEstimatorState(
          observationId,
          services.gmosSequenceService.selectGmosSouthStatic,
          services.gmosSequenceService.selectGmosSouthDynamicStep
        )

      private def stepRecordMap[D](
        observationId:  Observation.Id,
        dynamicConfigs: Stream[F, (Step.Id, D)]
      )(using Transaction[F]): F[Map[Step.Id, (D, StepConfig)]] = {

        // `configs` is a grouping of a configuration C and the set of steps
        // that share that same configuration value.  It's grouped this way to
        // minimize the amount of memory that is needed to hold the entire
        // executed part of the sequence.
        //
        // Here we want to flip this map around (Step.Id -> C), but also map C
        // to A if possible.  The mapping function is intended to lookup the
        // instrument configuration associated with a science, gcal, etc.
        // configuration.  E.g. (Step.Id, Science) => (GmosNorth, Science).
        def foldConfigs[A, C](
          configs: Iterable[(C, Set[Step.Id])],
          z:       Map[Step.Id, A]
        )(op: (Step.Id, C) => Option[A]): Map[Step.Id, A] =
          configs.foldLeft(z) { case (zʹ, (c, sids)) =>
            sids.foldLeft(zʹ) { case (zʹʹ, sid) =>
              op(sid, c).fold(zʹʹ)(a => zʹʹ + (sid -> a))
            }
          }

        def foldStream[A, C](
          stream: Stream[F, (Step.Id, C)],
          z:      Map[Step.Id, A]
        )(op: (Step.Id, C) => Option[A]): F[Map[Step.Id, A]] =
          stream.fold(Map.empty[C, Set[Step.Id]]) { case (m, (s, c)) =>
            m.updatedWith(c) { _.map(_ + s).orElse(Set(s).some)}
          }.compile.onlyOrError.map(foldConfigs(_, z)(op))

        def foldQuery[A, C](
          query: Query[Observation.Id, (Step.Id, C)],
          z:     Map[Step.Id, A]
        )(op: (Step.Id, C) => Option[A]): F[Map[Step.Id, A]] =
          foldStream(session.stream(query)(observationId, 1024), z)(op)

        for {
          ds <- foldStream(dynamicConfigs, Map.empty[Step.Id, D])((_, d) => d.some)
          r  <- foldQuery(Statements.SelectStepConfigForObs, Map.empty[Step.Id, (D, StepConfig)]) { (sid, sc) =>
            ds.get(sid).tupleRight(sc)
          }
        } yield r

      }

      // Want a map from atom configuration to completed count that can be
      // matched against the generated atoms.
      private def completedAtomMap[D](
        observationId: Observation.Id
      )(
        stepMap: Map[Step.Id, (D, StepConfig)]
      )(using Transaction[F]): F[CompletedAtomMap[D]] = {

        import CompletedAtomMap.AtomMatch
        import CompletedAtomMap.StepMatch

        trait State {
          def reset: State
          def next(aid: Atom.Id, count: NonNegShort, sequenceType: SequenceType, step: StepMatch[D]): State
          def finalized: CompletedAtomMap[D]
        }

        case class Reset(completed: CompletedAtomMap[D]) extends State {
          override def reset: State = this

          override def next(aid: Atom.Id, count: NonNegShort, sequenceType: SequenceType, step: StepMatch[D]): State =
            InProgress(aid, count, sequenceType, List(step), completed)

          override def finalized: CompletedAtomMap[D] = completed
        }

        object Reset {
          lazy val init: State = Reset(CompletedAtomMap.Empty)
        }

        case class InProgress(
          inProgressAtomId:       Atom.Id,
          inProgressCount:        NonNegShort,
          inProgressSequenceType: SequenceType,
          inProgressSteps:        AtomMatch[D],
          completed:              CompletedAtomMap[D]
        ) extends State {

          override def reset: State = Reset(completed)

          private def addStep(step: StepMatch[D]): InProgress =
            copy(inProgressSteps = step :: inProgressSteps)

          override def next(aid: Atom.Id, count: NonNegShort, sequenceType: SequenceType, step: StepMatch[D]): State =
            if (aid === inProgressAtomId) addStep(step) // continue existing atom
            else InProgress(aid, count, sequenceType, List(step), finalized) // start a new atom

          private def inProgressKey: CompletedAtomMap.Key[D] =
            CompletedAtomMap.Key(inProgressSequenceType, inProgressSteps.reverse)

          override def finalized: CompletedAtomMap[D] =
            if (inProgressSteps.sizeIs != inProgressCount.value) completed
            else completed.increment(inProgressKey)

        }

        // Fold over the stream of completed steps in completion order.  If an
        // atom is broken up by anything else it shouldn't count as complete.
        session
          .stream(Statements.SelectCompletedStepRecordsForObs)(observationId, 1024)
          .fold(Reset.init) { case (state, (aid, cnt, seqType, sid)) =>
            stepMap.get(sid).fold(state.reset)(state.next(aid, cnt, seqType, _))
          }
          .compile
          .onlyOrError
          .map(_.finalized)

      }

      override def setStepCompleted(
        stepId: Step.Id,
        time:   Option[Timestamp]
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.SetStepCompleted)(time, stepId).void

      override def insertAtomRecord(
        visitId:      Visit.Id,
        instrument:   Instrument,
        stepCount:    NonNegShort,
        sequenceType: SequenceType
      )(using Transaction[F]): F[InsertAtomResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(InsertAtomResponse.NotAuthorized.apply))
          v    = visitService.select(visitId).map(_.filter(_.instrument === instrument))
          inv <- EitherT.fromOptionF(v, InsertAtomResponse.VisitNotFound(visitId, instrument))
          aid <- EitherT.right[InsertAtomResponse](UUIDGen[F].randomUUID.map(Atom.Id.fromUuid))
          _   <- EitherT.right[InsertAtomResponse](session.execute(Statements.InsertAtom)(aid, inv.observationId, visitId, instrument, stepCount, sequenceType))
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

      private def insertStepRecord[S, D](
        atomId:              Atom.Id,
        instrument:          Instrument,
        stepConfig:          StepConfig,
        observeClass:        ObserveClass,
        timeEstimate:        (S, EstimatorState[D]) => StepEstimate,
        estimatorState:      Observation.Id => F[Option[(S, EstimatorState[D])]],
        insertDynamicConfig: Step.Id => F[Unit]
      )(using Transaction[F]): F[InsertStepResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          foo  = session.option(Statements.SelectObservationId)((atomId, instrument))
          fos  = OptionT(foo).flatMap(o => OptionT(estimatorState(o))).value
          sid <- EitherT.right(UUIDGen[F].randomUUID.map(Step.Id.fromUuid))
          es  <- EitherT.fromOptionF(fos, AtomNotFound(atomId, instrument))
          _   <- EitherT.right(session.execute(Statements.InsertStep)(
                   sid, atomId, instrument, stepConfig.stepType, observeClass, timeEstimate.tupled(es).total
                 )).void
          _   <- EitherT.right(insertStepConfig(sid, stepConfig))
          _   <- EitherT.right(insertDynamicConfig(sid))
        } yield Success(sid)).merge

      override def insertGmosNorthStepRecord(
        atomId:         Atom.Id,
        dynamicConfig:  GmosNorth,
        stepConfig:     StepConfig,
        observeClass:   ObserveClass,
        timeCalculator: TimeEstimateCalculator[GmosNorthStatic, GmosNorth]
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosNorth,
          stepConfig,
          observeClass,
          timeCalculator.estimateStep(_, _, ProtoStep(dynamicConfig, stepConfig, observeClass)),
          selectGmosNorthEstimatorState,
          sid => gmosSequenceService.insertGmosNorthDynamic(sid, dynamicConfig)
        )

      override def insertGmosSouthStepRecord(
        atomId:         Atom.Id,
        dynamicConfig:  GmosSouth,
        stepConfig:     StepConfig,
        observeClass:   ObserveClass,
        timeCalculator: TimeEstimateCalculator[GmosSouthStatic, GmosSouth]
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosSouth,
          stepConfig,
          observeClass,
          timeCalculator.estimateStep(_, _, ProtoStep(dynamicConfig, stepConfig, observeClass)),
          selectGmosSouthEstimatorState,
          sid => gmosSequenceService.insertGmosSouthDynamic(sid, dynamicConfig)
        )
    }

  object Statements {

    val SelectObservationId: Query[(Atom.Id, Instrument), Observation.Id] =
      sql"""
        SELECT c_observation_id
          FROM t_atom_record
         WHERE c_atom_id = $atom_id AND c_instrument = $instrument
      """.query(observation_id)

    val InsertAtom: Command[(
      Atom.Id,
      Observation.Id,
      Visit.Id,
      Instrument,
      NonNegShort,
      SequenceType
    )] =
      sql"""
        INSERT INTO t_atom_record (
          c_atom_id,
          c_observation_id,
          c_visit_id,
          c_instrument,
          c_step_count,
          c_sequence_type
        ) SELECT
          $atom_id,
          $observation_id,
          $visit_id,
          $instrument,
          $int2_nonneg,
          $sequence_type
      """.command

    val InsertStep: Command[(
      Step.Id,
      Atom.Id,
      Instrument,
      StepType,
      ObserveClass,
      TimeSpan
    )] =
      sql"""
        INSERT INTO t_step_record (
          c_step_id,
          c_atom_id,
          c_instrument,
          c_step_type,
          c_observe_class,
          c_time_estimate
        ) SELECT
          $step_id,
          $atom_id,
          $instrument,
          $step_type,
          $obs_class,
          $time_span
      """.command

    /**
     * Selects completed step records for a particular observation.  A completed
     * step is one for which the completion time has been set by the reception
     * of an EndStep step event and for which there are no pending datasets or
     * datasets which have a QA state set to anything other than Pass.
     */
    val SelectCompletedStepRecordsForObs: Query[Observation.Id, (Atom.Id, NonNegShort, SequenceType, Step.Id)] =
      (sql"""
        SELECT
          a.c_atom_id,
          a.c_step_count,
          a.c_sequence_type,
          s.c_step_id
        FROM
          t_step_record s
        INNER JOIN
          t_atom_record a
        ON a.c_atom_id = s.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id AND s.c_completed IS NOT NULL""" <~ sql"""
          AND NOT EXISTS (
            SELECT 1
            FROM   t_dataset d
            WHERE
              d.c_step_id = s.c_step_id
              AND (
                d.c_end_time IS NULL
                OR (d.c_qa_state IS NOT NULL AND d.c_qa_state <> 'Pass'::e_dataset_qa_state)
              )
          )
        ORDER BY s.c_completed
      """).query(atom_id *: int2_nonneg *: sequence_type *: step_id)

    def encodeColumns(prefix: Option[String], columns: List[String]): String =
      columns.map(c => s"${prefix.foldMap(_ + ".")}$c").intercalate(",\n")

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

    private val StepConfigScienceColumns: List[String] =
      List(
        "c_offset_p",
        "c_offset_q",
        "c_guide_state"
      )

    val InsertStepConfigScience: Command[(Step.Id, StepConfig.Science)] =
      sql"""
        ${insertStepConfigFragment("t_step_config_science", StepConfigScienceColumns)} SELECT
          $step_id,
          $step_config_science
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
        step_config_science.opt *:
        step_config_smart_gcal.opt
      ).eimap { case (stepType, oGcal, oScience, oSmart) =>
        stepType match {
          case StepType.Bias      => StepConfig.Bias.asRight
          case StepType.Dark      => StepConfig.Dark.asRight
          case StepType.Gcal      => oGcal.toRight("Missing gcal step config definition")
          case StepType.Science   => oScience.toRight("Missing science step config definition")
          case StepType.SmartGcal => oSmart.toRight("Missing smart gcal step config definition")
        }
      } { stepConfig =>
        (stepConfig.stepType,
         StepConfig.gcal.getOption(stepConfig),
         StepConfig.science.getOption(stepConfig),
         StepConfig.smartGcal.getOption(stepConfig)
        )
      }

    val SelectStepConfigForObs: Query[Observation.Id, (Step.Id, StepConfig)] =
      (sql"""
        SELECT
          v.c_step_id,
          v.c_step_type,
          #${encodeColumns("v".some, StepConfigGcalColumns)},
          #${encodeColumns("v".some, StepConfigScienceColumns)},
          #${encodeColumns("v".some, StepConfigSmartGcalColumns)}
        FROM v_step_record v
        INNER JOIN t_atom_record a ON a.c_atom_id = v.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id"""
      ).query(step_id *: step_config)

    val SetStepCompleted: Command[(Option[Timestamp], Step.Id)] =
      sql"""
        UPDATE t_step_record
           SET c_completed = ${core_timestamp.opt}
         WHERE c_step_id = $step_id
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

    val SelectLastScienceConfig: Query[Observation.Id, StepConfig.Science] =
      sql"""
        SELECT
          #${encodeColumns("s".some, StepConfigScienceColumns)}
        FROM v_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          a.c_observation_id = $observation_id AND
          s.c_step_type      = 'science'
        ORDER BY s.c_created DESC
        LIMIT 1
      """.query(step_config_science)

    val SelectLastStepConfig: Query[Observation.Id, (Step.Id, StepConfig, ObserveClass)] =
      sql"""
        SELECT
          s.c_step_id,
          s.c_step_type,
          #${encodeColumns("s".some, StepConfigGcalColumns)},
          #${encodeColumns("s".some, StepConfigScienceColumns)},
          #${encodeColumns("s".some, StepConfigSmartGcalColumns)},
          s.c_observe_class
        FROM v_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          a.c_observation_id = $observation_id
        ORDER BY s.c_created DESC
        LIMIT 1
      """.query(step_id *: step_config *: obs_class)

  }
}
