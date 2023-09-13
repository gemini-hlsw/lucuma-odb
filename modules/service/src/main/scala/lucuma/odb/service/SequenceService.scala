// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
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

  def selectGmosNorthAtomCounts(
    observationId: Observation.Id
  )(using Transaction[F]): F[Map[List[(GmosNorth, StepConfig)], PosInt]]

  def selectGmosNorthSteps(
    observationId: Observation.Id
  )(using Transaction[F]): F[Map[Step.Id, (GmosNorth, StepConfig)]]

  def selectGmosSouthAtomCounts(
    observationId: Observation.Id
  )(using Transaction[F]): F[Map[List[(GmosSouth, StepConfig)], PosInt]]

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
    atomId:     Atom.Id,
    instrument: GmosNorth,
    step:       StepConfig
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

  def insertGmosSouthStepRecord(
    atomId:     Atom.Id,
    instrument: GmosSouth,
    step:       StepConfig
  )(using Transaction[F]): F[SequenceService.InsertStepResponse]

}

object SequenceService {

  case class AtomRecord(
    atomId:        Atom.Id,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    instrument:    Instrument,
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

      override def selectGmosNorthAtomCounts(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[List[(GmosNorth, StepConfig)], PosInt]] =
        selectAtomCounts(
          observationId,
          gmosSequenceService.selectGmosNorthDynamic(observationId)
        )

      override def selectGmosSouthAtomCounts(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[List[(GmosSouth, StepConfig)], PosInt]] =
        selectAtomCounts(
          observationId,
          gmosSequenceService.selectGmosSouthDynamic(observationId)
        )

      private def selectAtomCounts[D](
        observationId:  Observation.Id,
        dynamicConfigs: Stream[F, (Step.Id, D)]
      )(using Transaction[F]): F[Map[List[(D, StepConfig)], PosInt]] =
        stepRecordMap(observationId, dynamicConfigs)
          .flatMap(completedAtomCountMap(observationId))

      def selectGmosNorthSteps(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[Step.Id, (GmosNorth, StepConfig)]] =
        stepRecordMap(observationId, gmosSequenceService.selectGmosNorthDynamic(observationId))

      def selectGmosSouthSteps(
        observationId: Observation.Id
      )(using Transaction[F]): F[Map[Step.Id, (GmosSouth, StepConfig)]] =
        stepRecordMap(observationId, gmosSequenceService.selectGmosSouthDynamic(observationId))

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
          e   = Map.empty[Step.Id, (D, StepConfig)]
          op  = (sid: Step.Id, sc: StepConfig) => ds.get(sid).tupleRight(sc)
          g  <- foldQuery(Statements.SelectStepConfigGcalForObs,      e)(op)
          s  <- foldQuery(Statements.SelectStepConfigScienceForObs,   g)(op)
          m  <- foldQuery(Statements.SelectStepConfigSmartGcalForObs, s)(op)
          bd <- biasAndDark(observationId)
        } yield foldConfigs(bd, m)(op)

      }

      // Bias and Dark have no real configuration other than the instrument
      // itself.  There's not a separate table for these like there is for gcal,
      // science, and smart gcal.
      private def biasAndDark(observationId: Observation.Id): F[List[(StepConfig, Set[Step.Id])]] =
        session
          .stream(Statements.SelectStepConfigBiasOrDarkForObs)(observationId, 1024)
          .fold((Set.empty[Step.Id], Set.empty[Step.Id])) { case ((bias, dark), (s, stype)) =>
            stype match {
              case StepType.Bias => (bias + s, dark)
              case StepType.Dark => (bias, dark + s)
              case _             => (bias, dark)
            }
          }
          .compile
          .onlyOrError
          .map { case (bias, dark) => List(StepConfig.Bias -> bias, StepConfig.Dark -> dark) }

      // Want a map from atom configuration to completed count that can be
      // matched against the generated atoms.
      private def completedAtomCountMap[D](
        observationId: Observation.Id
      )(
        stepMap: Map[Step.Id, (D, StepConfig)]
      )(using Transaction[F]): F[Map[List[(D, StepConfig)], PosInt]] = {

        type StepMatch     = (D, StepConfig)
        type AtomMatch     = List[StepMatch]
        type CompletionMap = Map[AtomMatch, PosInt]

        trait State {
          def reset: State
          def next(aid: Atom.Id, count: NonNegShort, step: StepMatch): State
          def finalized: CompletionMap
        }

        case class Reset(completed: CompletionMap) extends State {
          def reset: State = this

          def next(aid: Atom.Id, count: NonNegShort, step: StepMatch): State =
            InProgress(aid, count, List(step), completed)

          def finalized: CompletionMap = completed
        }

        object Reset {
          lazy val init: State = Reset(Map.empty)
        }

        case class InProgress(
          inProgressAtomId: Atom.Id,
          inProgressCount:  NonNegShort,
          inProgressSteps:  AtomMatch,
          completed:        CompletionMap
        ) extends State {

          def reset: State = Reset(completed)

          def next(aid: Atom.Id, count: NonNegShort, step: StepMatch): State =
            if (aid === inProgressAtomId)
              copy(inProgressSteps = step :: inProgressSteps) // continue existing atom
            else
              InProgress(aid, count, List(step), finalized)   // start a new atom

          def finalized: CompletionMap =
            if (inProgressSteps.sizeIs != inProgressCount.value)
              completed
            else
              completed.updatedWith(inProgressSteps.reverse)(
                _.flatMap(posInt => PosInt.from(posInt.value + 1).toOption)
                 .orElse(PosInt.from(1).toOption)
              )
        }

        // Fold over the stream of completed steps in completion order.  If an
        // atom is broken up by anything else it shouldn't count as complete.
        session
          .stream(Statements.SelectCompletedStepRecordsForObs)(observationId, 1024)
          .fold(Reset.init) { case (state, (aid, cnt, sid)) =>
            stepMap.get(sid).fold(state.reset)(state.next(aid, cnt, _))
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

      private def insertStepRecord(
        atomId:              Atom.Id,
        instrument:          Instrument,
        stepConfig:          StepConfig,
        insertDynamicConfig: Step.Id => F[Unit]
      )(using Transaction[F]): F[InsertStepResponse] =
        (for {
          _   <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          a    = selectAtomRecord(atomId).map(_.filter(_.instrument === instrument))
          _   <- EitherT.fromOptionF(a, AtomNotFound(atomId, instrument))
          sid <- EitherT.right[InsertStepResponse](UUIDGen[F].randomUUID.map(Step.Id.fromUuid))
          _   <- EitherT.right[InsertStepResponse](session.execute(Statements.InsertStep)(sid, atomId, instrument, stepConfig.stepType)).void
          _   <- EitherT.right(insertStepConfig(sid, stepConfig))
          _   <- EitherT.right(insertDynamicConfig(sid))
        } yield Success(sid)).merge

      override def insertGmosNorthStepRecord(
        atomId:        Atom.Id,
        dynamicConfig: GmosNorth,
        stepConfig:    StepConfig
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosNorth,
          stepConfig,
          sid => gmosSequenceService.insertGmosNorthDynamic(sid, dynamicConfig)
        )

      override def insertGmosSouthStepRecord(
        atomId:        Atom.Id,
        dynamicConfig: GmosSouth,
        stepConfig:    StepConfig
      )(using Transaction[F]): F[SequenceService.InsertStepResponse] =
        insertStepRecord(
          atomId,
          Instrument.GmosSouth,
          stepConfig,
          sid => gmosSequenceService.insertGmosSouthDynamic(sid, dynamicConfig)
        )

    }

  object Statements {

    private val atom_record: Codec[AtomRecord] =
      (
        atom_id        *:
        observation_id *:
        visit_id       *:
        instrument     *:
        int2_nonneg    *:
        sequence_type  *:
        core_timestamp
      ).to[AtomRecord]

    val SelectAtom: Query[Atom.Id, AtomRecord] =
      sql"""
        SELECT
          c_atom_id,
          c_observation_id,
          c_visit_id,
          c_instrument,
          c_step_count,
          c_sequence_type,
          c_created
        FROM t_atom_record
        WHERE c_atom_id = $atom_id
      """.query(atom_record)

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
      StepType
    )] =
      sql"""
        INSERT INTO t_step_record (
          c_step_id,
          c_atom_id,
          c_instrument,
          c_step_type
        ) SELECT
          $step_id,
          $atom_id,
          $instrument,
          $step_type
      """.command

    val SelectCompletedStepRecordsForObs: Query[Observation.Id, (Atom.Id, NonNegShort, Step.Id)] =
      (sql"""
        SELECT
          a.c_atom_id,
          a.c_step_count,
          s.c_step_id
        FROM t_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id AND s.c_completed IS NOT NULL ORDER BY s.c_completed"""
      ).query(atom_id *: int2_nonneg *: step_id)

    val SelectStepConfigBiasOrDarkForObs: Query[Observation.Id, (Step.Id, StepType)] =
      (sql"""
        SELECT
          s.c_step_id,
          s.c_step_type
        FROM t_step_record s
        INNER JOIN t_atom_record a on a.c_atom_id = s.c_atom_id
        WHERE
      """ ~> sql"""
          a.c_observation_id = $observation_id AND
          (s.c_step_type = 'bias' OR s.c_step_type = 'dark')
      """).query(step_id *: step_type)

    def encodeColumns(prefix: Option[String], columns: List[String]): String =
      columns.map(c => s"${prefix.foldMap(_ + ".")}$c").intercalate(",\n")

    private def selectStepConfigForObs[A](table: String, columns: List[String], decoderA: Decoder[A]): Query[Observation.Id, (Step.Id, A)] =
      (sql"""
        SELECT
          s.c_step_id,
          #${encodeColumns("c".some, columns)}
        FROM #$table c
        INNER JOIN t_step_record s ON s.c_step_id = c.c_step_id
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE """ ~> sql"""a.c_observation_id = $observation_id"""
      ).query(step_id *: decoderA)

// TODO: I would like to write the insert step config logic once, parameterized
// by the table name, columns, and the encoder.  I think I'm being thwarted by
// scala.quoted / macro issues?
//
//[error] /Users/swalker/dev/lucuma-odb/modules/core/shared/src/main/scala-3/syntax/StringContextOps.scala: Found:    skunk.Encoder[A]
//[error] Required: skunk.Encoder[Nothing *: Nothing]
//[error] one error found
//[error] (service / Compile / compileIncremental) Compilation failed
//
// is there a way to get this method the context information that the macro is expecting?
//
//    def insertStepConfig[A: Type](table: String, columns: List[String], encoderA: Encoder[A]): Command[(Step.Id, A)] = {
//      val f: Fragment[Void] = sql"""
//        INSERT INTO #$table (
//          c_step_id,
//          #${encodeColumns(None, columns)}
//        )"""
//
//      sql"""
//        $f
//        SELECT
//          $step_id,
//          $encoderA
//      """.command
//    }

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
//      insertStepConfig[StepConfig.Gcal]("t_step_config_gcal", StepConfigGcalColumns, step_config_gcal)
      sql"""
        ${insertStepConfigFragment("t_step_config_gcal", StepConfigGcalColumns)} SELECT
          $step_id,
          $step_config_gcal
      """.command

    val SelectStepConfigGcalForObs: Query[Observation.Id, (Step.Id, StepConfig.Gcal)] =
      selectStepConfigForObs("t_step_config_gcal", StepConfigGcalColumns, step_config_gcal)

    private val StepConfigScienceColumns: List[String] =
      List(
        "c_offset_p",
        "c_offset_q",
        "c_guide_state"
      )

    val InsertStepConfigScience: Command[(Step.Id, StepConfig.Science)] =
//      insertStepConfig[StepConfig.Science]("t_step_config_science", StepConfigGcalColumns, step_config_science)
      sql"""
        ${insertStepConfigFragment("t_step_config_science", StepConfigScienceColumns)} SELECT
          $step_id,
          $step_config_science
      """.command

    val SelectStepConfigScienceForObs: Query[Observation.Id, (Step.Id, StepConfig.Science)] =
      selectStepConfigForObs("t_step_config_science", StepConfigScienceColumns, step_config_science)

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

    val SelectStepConfigSmartGcalForObs: Query[Observation.Id, (Step.Id, StepConfig.SmartGcal)] =
      selectStepConfigForObs("t_step_config_smart_gcal", StepConfigSmartGcalColumns, step_config_smart_gcal)

    val SetStepCompleted: Command[(Option[Timestamp], Step.Id)] =
      sql"""
        UPDATE t_step_record
           SET c_completed = ${core_timestamp.opt}
         WHERE c_step_id = $step_id
      """.command

  }
}
