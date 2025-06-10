// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
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

/**
 * Flamingos 2 long slit acquisition.
 */
object Acquisition:

  extension (scienceFilter: Flamingos2Filter)
    def toAcquisitionFilter: Flamingos2Filter =
      Flamingos2Filter.acquisition.toList.minBy(f => scienceFilter.wavelength.diff(f.wavelength).abs)

  case class Steps(
    image: ProtoStep[F2],
    p10:   ProtoStep[F2],
    slit:  ProtoStep[F2]
  ):
    val initialAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(image, p10, slit.withBreakpoint)

    val repeatingAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(slit)

  private object StepComputer extends SequenceState[F2] with Flamingos2InitialDynamicConfig:
    def compute(
      exposureTime:  TimeSpan,
      scienceFilter: Flamingos2Filter,
      builtin:       Flamingos2Fpu
    ): Steps =
      eval:
        for
          _  <- F2.exposure    := exposureTime
          _  <- F2.disperser   := none[Flamingos2Disperser]
          _  <- F2.filter      := scienceFilter.toAcquisitionFilter
          _  <- F2.readMode    := Flamingos2ReadMode.forExposureTime(exposureTime)
          _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
          _  <- F2.fpu         := Flamingos2FpuMask.Imaging
          _  <- F2.decker      := Flamingos2FpuMask.Imaging.defaultDecker
          _  <- F2.readoutMode := Flamingos2ReadoutMode.Science
          _  <- F2.reads       := Flamingos2ReadMode.forExposureTime(exposureTime).readCount
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- F2.exposure    := 10.secondTimeSpan  // Fixed
          _  <- F2.fpu         := Flamingos2FpuMask.Builtin(builtin)
          _  <- F2.decker      := Flamingos2FpuMask.Builtin(builtin).defaultDecker
          s1 <- scienceStep(10.arcsec, 0.arcsec, ObserveClass.Acquisition)

          s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Steps(s0, s1, s2)

  private sealed trait AcquisitionState extends SequenceGenerator.Base[F2]:

    def builder: AtomBuilder[F2]
    def steps: Steps
    def recordCompleted(step: StepRecord[F2]): AcquisitionState
    def calcState: TimeEstimateCalculator.Last[F2]
    def tracker: IndexTracker

    def updateTracker(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker): AcquisitionState

    override def recordStep(step: StepRecord[F2])(using Eq[F2]): SequenceGenerator[F2] =
      if step.isScienceSequence then this
      else
        val a = updateTracker(calcState.next(step.protoStep), tracker.record(step))
        if step.successfullyCompleted then a.recordCompleted(step) else a

  private object AcquisitionState:

    def initialAcq(
      builder: AtomBuilder[F2],
      steps:   NonEmptyList[ProtoStep[F2]],
      aix:     Int,
      six:     Int
    ): State[TimeEstimateCalculator.Last[F2], Atom[F2]] =
      builder.build(NonEmptyString.unapply("Initial Acquisition"), aix, six, steps)

    def fineAdjustments(
      builder: AtomBuilder[F2],
      slit:    ProtoStep[F2],
      aix:     Int
    ): State[TimeEstimateCalculator.Last[F2], Atom[F2]] =
      builder.build(NonEmptyString.unapply("Fine Adjustments"), aix, 1, NonEmptyList.one(slit))

    def gen(
      builder:   AtomBuilder[F2],
      init:      Option[NonEmptyList[ProtoStep[F2]]],
      slit:      ProtoStep[F2],
      calcState: TimeEstimateCalculator.Last[F2],
      track:     IndexTracker
    ): Stream[Pure, Atom[F2]] =
      (for
        a0 <- init.fold(fineAdjustments(builder, slit, track.atomCount)): nel =>
                initialAcq(builder, nel, track.atomCount, track.stepCount)
        a1 <- fineAdjustments(builder, slit, track.atomCount+1)
      yield Stream(a0, a1)).runA(calcState).value

    case class Init(lastReset: Option[Timestamp], tracker: IndexTracker, builder: AtomBuilder[F2], steps: Steps) extends SequenceGenerator.Base[F2]:
      override def generate(ignore: Timestamp): Stream[Pure, Atom[F2]] =
        gen(builder, steps.initialAtom.some, steps.slit, TimeEstimateCalculator.Last.empty[F2], tracker)

      override def recordStep(step: StepRecord[F2])(using Eq[F2]): SequenceGenerator[F2] =
        if step.isScienceSequence then this
        else if lastReset.exists(_ > step.created) then copy(tracker = tracker.record(step))
        else ExpectImage(TimeEstimateCalculator.Last.empty[F2], tracker, builder, steps).recordStep(step)

    case class ExpectImage(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker, builder: AtomBuilder[F2], steps: Steps) extends AcquisitionState:
      override def generate(ignore: Timestamp): Stream[Pure, Atom[F2]] =
        gen(builder, steps.initialAtom.some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker): AcquisitionState =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[F2]): AcquisitionState =
        if steps.image.matches(step) then ExpectP10(calcState, tracker, builder, steps)
        else this

    case class ExpectP10(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker, builder: AtomBuilder[F2], steps: Steps) extends AcquisitionState:
      override def generate(ignore: Timestamp): Stream[Pure, Atom[F2]] =
        gen(builder, NonEmptyList.of(steps.p10, steps.slit.withBreakpoint).some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker): AcquisitionState =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[F2]): AcquisitionState =
        if steps.p10.matches(step) then ExpectSlit(calcState, tracker, builder, steps, initialAtom = true)
        else this

    case class ExpectSlit(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker, builder: AtomBuilder[F2], steps: Steps, initialAtom: Boolean) extends AcquisitionState:

      override def generate(ignore: Timestamp): Stream[Pure, Atom[F2]] =
        gen(builder, Option.when(initialAtom)(NonEmptyList.one(steps.slit.withBreakpoint)), steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[F2], tracker: IndexTracker): AcquisitionState =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[F2]): AcquisitionState =
        if steps.slit.matches(step) then ExpectSlit(calcState, tracker, builder, steps, initialAtom = false)
        else this

  def instantiate(
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime],
    lastReset:     Option[Timestamp]
  ): Either[OdbError, SequenceGenerator[F2]] =
    time
      .filterOrElse(_.exposureTime.toNonNegMicroseconds.value > 0, OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: Flamingos 2 Long Slit requires a positive exposure time.".some))
      .map: t =>
        AcquisitionState.Init(
          lastReset,
          IndexTracker.Zero,
          AtomBuilder.instantiate(
            estimator,
            static,
            namespace,
            SequenceType.Acquisition
          ),
          StepComputer.compute(t.exposureTime, config.filter, config.fpu)
        )
