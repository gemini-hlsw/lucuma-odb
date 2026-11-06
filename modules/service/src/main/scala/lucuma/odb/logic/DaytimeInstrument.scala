// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.syntax.option.*
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.StepCheck
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.StaticConfigs
import lucuma.odb.service.SequenceService
import lucuma.odb.service.Services
import skunk.Transaction

/**
 * What the daytime calibration paths need from one instrument, gathered so
 * that reading, generating, estimating and materializing a daytime sequence
 * dispatch on the instrument once.
 */
trait DaytimeInstrument[F[_]]:
  type S
  type D

  def static:    S
  def estimator: StepTimeEstimateCalculator[S, D]
  def expander:  SmartGcalExpander[F, S, D]
  def stepCheck: StepCheck[D]
  def sets(config: Config): List[CalibrationSet[D]]

  /** The typed execution config wrapped in the instrument sum. */
  def instrumentExecutionConfig(config: ExecutionConfig[S, D]): InstrumentExecutionConfig

  def select(
    oid:          Observation.Id,
    sequenceType: SequenceType
  )(using Transaction[F]): F[Option[Stream[F, Atom[D]]]]

  def materialize(
    oid:           Observation.Id,
    stream:        StreamingExecutionConfig[F, S, D],
    sequenceTypes: Set[SequenceType]
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

object DaytimeInstrument:

  private def instance[F[_], S0, D0: StepCheck](
    static0:      S0,
    estimator0:   StepTimeEstimateCalculator[S0, D0],
    expander0:    SmartGcalExpander[F, S0, D0],
    sets0:        Config => List[CalibrationSet[D0]],
    wrap0:        ExecutionConfig[S0, D0] => InstrumentExecutionConfig,
    select0:      (Observation.Id, SequenceType, S0) => Transaction[F] ?=> F[Option[Stream[F, Atom[D0]]]],
    materialize0: (Observation.Id, StreamingExecutionConfig[F, S0, D0], Set[SequenceType]) => (Transaction[F], Services.ServiceAccess) ?=> F[Unit]
  ): DaytimeInstrument[F] =
    new DaytimeInstrument[F]:
      type S = S0
      type D = D0

      val static    = static0
      val estimator = estimator0
      val expander  = expander0
      val stepCheck = StepCheck[D0]

      def sets(config: Config): List[CalibrationSet[D0]] =
        sets0(config)

      def instrumentExecutionConfig(config: ExecutionConfig[S0, D0]): InstrumentExecutionConfig =
        wrap0(config)

      def select(
        oid:          Observation.Id,
        sequenceType: SequenceType
      )(using Transaction[F]): F[Option[Stream[F, Atom[D0]]]] =
        select0(oid, sequenceType, static0)

      def materialize(
        oid:           Observation.Id,
        stream:        StreamingExecutionConfig[F, S0, D0],
        sequenceTypes: Set[SequenceType]
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        materialize0(oid, stream, sequenceTypes)

  /** The instrument's daytime pieces, or none for an instrument without daytime calibrations. */
  def forInstrument[F[_]](
    instrument: Instrument,
    calculator: TimeEstimateCalculatorImplementation.ForInstrumentMode,
    exp:        SmartGcalImplementation.ForInstrument[F],
    sequence:   SequenceService[F]
  ): Option[DaytimeInstrument[F]] =
    instrument match
      case Instrument.Flamingos2 =>
        instance(
          StaticConfigs.Flamingos2,
          calculator.flamingos2Step,
          exp.flamingos2,
          _.flamingos2Sets,
          InstrumentExecutionConfig.Flamingos2.apply,
          sequence.selectFlamingos2Sequence(_, _, _),
          sequence.materializeFlamingos2ExecutionConfig(_, _, _)
        ).some
      case Instrument.Ghost =>
        instance(
          StaticConfigs.Ghost,
          calculator.ghostStep,
          exp.ghost,
          _.ghostSets,
          InstrumentExecutionConfig.Ghost.apply,
          sequence.selectGhostSequence(_, _, _),
          sequence.materializeGhostExecutionConfig(_, _, _)
        ).some
      case Instrument.GmosNorth =>
        instance(
          StaticConfigs.GmosNorth,
          calculator.gmosNorthStep,
          exp.gmosNorth,
          _.gmosNorthSets,
          InstrumentExecutionConfig.GmosNorth.apply,
          sequence.selectGmosNorthSequence(_, _, _),
          sequence.materializeGmosNorthExecutionConfig(_, _, _)
        ).some
      case Instrument.GmosSouth =>
        instance(
          StaticConfigs.GmosSouth,
          calculator.gmosSouthStep,
          exp.gmosSouth,
          _.gmosSouthSets,
          InstrumentExecutionConfig.GmosSouth.apply,
          sequence.selectGmosSouthSequence(_, _, _),
          sequence.materializeGmosSouthExecutionConfig(_, _, _)
        ).some
      case Instrument.Gnirs =>
        instance(
          StaticConfigs.Gnirs,
          calculator.gnirsStep,
          exp.gnirs,
          _.gnirsSets,
          InstrumentExecutionConfig.Gnirs.apply,
          sequence.selectGnirsSequence(_, _, _),
          sequence.materializeGnirsExecutionConfig(_, _, _)
        ).some
      case Instrument.Igrins2 =>
        instance(
          StaticConfigs.Igrins2,
          calculator.igrins2Step,
          exp.igrins2,
          _.igrins2Sets,
          InstrumentExecutionConfig.Igrins2.apply,
          sequence.selectIgrins2Sequence(_, _, _),
          sequence.materializeIgrins2ExecutionConfig(_, _, _)
        ).some
      case _ =>
        none
