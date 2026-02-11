// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker

import java.util.UUID

object Acquisition:
  val AcquisitionSN: SignalToNoise =
    SignalToNoise.FromBigDecimalExact.getOption(10).get

  val MaxExpTimeLastStep = 360.secondTimeSpan

  def filter[L](acqFilters: NonEmptyList[L], λ: Wavelength, wavelength: L => Wavelength): L =
    acqFilters.toList.minBy(filter => λ.diff(wavelength(filter)).abs)

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  case class Steps[D](
    ccd2: ProtoStep[D],
    p10:  ProtoStep[D],
    slit: ProtoStep[D]
  ):
    val initialAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(ccd2, p10, slit.withBreakpoint)

    val repeatingAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(slit)

  private sealed trait StepComputer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def compute(
      acqConfig:    AcquisitionConfig[L],
      fpu:          U,
      exposureTime: TimeSpan
    ): Acquisition.Steps[D] =

      // Last step, max 360s
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images#activity-2516
      def lastExpTime(exposureTime: TimeSpan): TimeSpan =
        Acquisition.MaxExpTimeLastStep min
          TimeSpan.unsafeFromMicroseconds(exposureTime.toMicroseconds * 3)

      eval:
        for
          _  <- optics.exposure      := exposureTime
          _  <- optics.filter        := acqConfig.filter.some
          _  <- optics.fpu           := none[GmosFpuMask[U]]
          _  <- optics.grating       := none[(G, GmosGratingOrder, Wavelength)]
          _  <- optics.xBin          := GmosXBinning.Two
          _  <- optics.yBin          := GmosYBinning.Two
          _  <- optics.roi           := acqConfig.roi.imagingRoi
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := 20.secondTimeSpan
          _  <- optics.fpu           := GmosFpuMask.Builtin(fpu).some
          _  <- optics.xBin          := GmosXBinning.One
          _  <- optics.yBin          := GmosYBinning.One
          _  <- optics.roi           := acqConfig.roi.slitRoi
          s1 <- scienceStep(10.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := lastExpTime(exposureTime)
          s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Acquisition.Steps(s0, s1, s2)

    end compute
  end StepComputer

  private object StepComputer:

    object North extends GmosNorthSequenceState
                    with StepComputer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

    object South extends GmosSouthSequenceState
                    with StepComputer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  end StepComputer

  private sealed trait AcquisitionState[D] extends SequenceGenerator.Base[D]:

    def builder: AtomBuilder[D]
    def steps: Steps[D]
    def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D]
    def calcState: TimeEstimateCalculator.Last[D]
    def tracker: IndexTracker

    def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D]

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      if step.isScienceSequence then this
      else
        val a = updateTracker(calcState.next(step.protoStep), tracker.record(step))
        if step.successfullyCompleted then a.recordCompleted(step) else a

  end AcquisitionState

  private object AcquisitionState:

    def initialAcq[D](
      builder: AtomBuilder[D],
      steps:   NonEmptyList[ProtoStep[D]],
      aix:     Int,
      six:     Int
    ): State[TimeEstimateCalculator.Last[D], Atom[D]] =
      builder.build(
        NonEmptyString.unapply("Initial Acquisition"),
        aix,
        six,
        steps
      )

    def fineAdjustments[D](
      builder: AtomBuilder[D],
      slit:    ProtoStep[D],
      aix:     Int
    ): State[TimeEstimateCalculator.Last[D], Atom[D]] =
      builder.build(
        NonEmptyString.unapply("Fine Adjustments"),
        aix,
        1,
        NonEmptyList.one(slit)
      )

    def gen[D](
      builder:   AtomBuilder[D],
      init:      Option[NonEmptyList[ProtoStep[D]]],
      slit:      ProtoStep[D],
      calcState: TimeEstimateCalculator.Last[D],
      track:     IndexTracker
    ): Stream[Pure, Atom[D]] =
      (for {
        a0 <- init.fold(fineAdjustments(builder, slit, track.atomCount)) { nel =>
          initialAcq(builder, nel, track.atomCount, track.stepCount)
        }
        a1 <- fineAdjustments(builder, slit, track.atomCount+1)
      } yield Stream(a0, a1)).runA(calcState).value

    case class Init[D](lastReset: Option[Timestamp], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D]) extends SequenceGenerator.Base[D]:

      override def generate: Stream[Pure, Atom[D]] =
        gen(builder, steps.initialAtom.some, steps.slit, TimeEstimateCalculator.Last.empty[D], tracker)

      override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
        if step.isScienceSequence then this
        else if lastReset.exists(_ > step.created) then copy(tracker = tracker.record(step))
        else ExpectCcd2(TimeEstimateCalculator.Last.empty[D], tracker, builder, steps).recordStep(step)

    end Init

    case class ExpectCcd2[D](calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D]) extends AcquisitionState[D]:

      override def generate: Stream[Pure, Atom[D]] =
        gen(builder, steps.initialAtom.some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.ccd2.matches(step) then ExpectP10(calcState, tracker, builder, steps)
        else this

    end ExpectCcd2

    case class ExpectP10[D](calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D]) extends AcquisitionState[D]:

      override def generate: Stream[Pure, Atom[D]] =
        gen(builder, NonEmptyList.of(steps.p10, steps.slit.withBreakpoint).some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.p10.matches(step) then ExpectSlit(calcState, tracker, builder, steps, initialAtom = true)
        else this

    end ExpectP10

    case class ExpectSlit[D](calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D], initialAtom: Boolean) extends AcquisitionState[D]:

      override def generate: Stream[Pure, Atom[D]] =
        gen(builder, Option.when(initialAtom)(NonEmptyList.one(steps.slit.withBreakpoint)), steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.slit.matches(step) then ExpectSlit(calcState, tracker, builder, steps, initialAtom = false)
        else this

    end ExpectSlit


  end AcquisitionState

  private def instantiate[D, G, L, U](
    oid:         Observation.Id,
    stepComp:    StepComputer[D, G, L, U],
    time:        Either[OdbError, IntegrationTime],
    calRole:     Option[CalibrationRole],
    atomBuilder: AtomBuilder[D],
    config:      Config[G, L, U],
    lastReset:   Option[Timestamp]
  ): Either[OdbError, SequenceGenerator[D]] =
    calRole match
      case Some(CalibrationRole.Twilight) =>
        SequenceGenerator.empty.asRight

      case _                              =>
        time
          .filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: GMOS Long Slit acquisition requires a positive exposure time.".some))
          .map: t =>
             AcquisitionState.Init(
               lastReset,
               IndexTracker.Zero,
               atomBuilder,
               stepComp.compute(config.acquisition, config.fpu, t.exposureTime)
             )

  def gmosNorth(
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    config:        Config.GmosNorth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole],
    lastReset:     Option[Timestamp]
  ): Either[OdbError, SequenceGenerator[GmosNorth]] =
    instantiate(
      observationId,
      StepComputer.North,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      config,
      lastReset
    )

  def gmosSouth(
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    config:        Config.GmosSouth,
    time:          Either[OdbError, IntegrationTime],
    calRole:       Option[CalibrationRole],
    lastReset:     Option[Timestamp]
  ): Either[OdbError, SequenceGenerator[GmosSouth]] =
    instantiate(
      observationId,
      StepComputer.South,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      config,
      lastReset
    )

end Acquisition