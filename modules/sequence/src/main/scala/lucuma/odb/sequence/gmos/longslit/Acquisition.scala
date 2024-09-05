// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Visit
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.itc.TargetIntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.util.IndexTracker
import lucuma.refined.*

object Acquisition:
  val AcquisitionSN: SignalToNoise =
    SignalToNoise.FromBigDecimalExact.getOption(10).get

  val DefaultIntegrationTime: TargetIntegrationTime =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 1.refined, AcquisitionSN)),
      Band.R // Band is meaningless here, but we need to provide one
    )

  val MinExposureTime    =   1.secondTimeSpan
  val MaxExposureTime    = 180.secondTimeSpan
  val MaxExpTimeLastStep = 360.secondTimeSpan

  def filter[L](acqFilters: NonEmptyList[L], λ: Wavelength, wavelength: L => Wavelength): L =
    acqFilters.toList.minBy { filter => λ.diff(wavelength(filter)).abs }

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
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[ProtoStep[D]] =
      NonEmptyList.of(slit)

  private sealed trait StepComputer[D, G, L, U] extends GmosSequenceState[D, G, L, U]:

    def wavelength(filter: L): Wavelength

    def compute(
      acqFilters:   NonEmptyList[L],
      fpu:          U,
      exposureTime: TimeSpan,
      λ:            Wavelength
    ): Acquisition.Steps[D] =

      val filter: L = Acquisition.filter(acqFilters, λ, wavelength)

      // Last step, max 360s
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images#activity-2516
      def lastExpTime(exposureTime: TimeSpan): TimeSpan =
        Acquisition.MaxExpTimeLastStep min
          TimeSpan.unsafeFromMicroseconds(exposureTime.toMicroseconds * 3)

      eval {
        for {
          _  <- optics.exposure      := exposureTime
          _  <- optics.filter        := filter.some
          _  <- optics.fpu           := none[GmosFpuMask[U]]
          _  <- optics.grating       := none[(G, GmosGratingOrder, Wavelength)]
          _  <- optics.xBin          := GmosXBinning.Two
          _  <- optics.yBin          := GmosYBinning.Two
          _  <- optics.roi           := GmosRoi.Ccd2
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := 20.secondTimeSpan
          _  <- optics.fpu           := GmosFpuMask.Builtin(fpu).some
          _  <- optics.xBin          := GmosXBinning.One
          _  <- optics.yBin          := GmosYBinning.One
          _  <- optics.roi           := GmosRoi.CentralStamp
          s1 <- scienceStep(10.arcsec, 0.arcsec, ObserveClass.Acquisition)

          _  <- optics.exposure      := lastExpTime(exposureTime)
          s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)

        } yield Acquisition.Steps(s0, s1, s2)
      }

    end compute
  end StepComputer

  private object StepComputer:

    object North extends GmosNorthSequenceState
                    with StepComputer[GmosNorth, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:

      override def wavelength(f: GmosNorthFilter): Wavelength =
        f.wavelength

    end North

    object South extends GmosSouthSequenceState
                    with StepComputer[GmosSouth, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:

      override def wavelength(f: GmosSouthFilter): Wavelength =
        f.wavelength

    end South

  end StepComputer

  private sealed trait AcquisitionState[D] extends SequenceGenerator[D]:

    def visitId: Visit.Id
    def steps: Steps[D]
    def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D]
    def tracker: IndexTracker

    def updateTracker(tracker: IndexTracker): AcquisitionState[D]

    def updatesVisit(step: StepRecord[D]): Boolean =
      step.visitId =!= visitId

    def reset(visitId: Visit.Id): AcquisitionState[D] =
      AcquisitionState.ExpectCcd2(visitId, tracker, steps)

    override def record(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      if updatesVisit(step) then
        reset(step.visitId).record(step)
      else
        val a = updateTracker(tracker.record(step))
        if !step.isAcquisitionSequence     then a.reset(step.visitId)
        else if step.successfullyCompleted then a.recordCompleted(step)
        else a

  end AcquisitionState

  private object AcquisitionState:

    def initialAcq[D](
      steps:   NonEmptyList[ProtoStep[D]],
      tracker: IndexTracker
    ): ProtoAtom[(ProtoStep[D], Int)] =
      ProtoAtom(
        NonEmptyString.unapply("Initial Acquisition"),
        steps.zipWithIndex.map(_.map(_ + tracker.stepCount))
      )

    def fineAdjustments[D](slit: ProtoStep[D]): ProtoAtom[(ProtoStep[D], Int)] =
      ProtoAtom(NonEmptyString.unapply("Fine Adjustments"), NonEmptyList.of((slit, 1)))

    def gen[D](
      init: Option[NonEmptyList[ProtoStep[D]]],
      slit: ProtoStep[D],
      track: IndexTracker
    ): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
      Stream(
        (init.fold(fineAdjustments(slit))(nel => initialAcq(nel, track)), track.atomCount),
        (fineAdjustments(slit), track.atomCount+1)
      )

    case class Init[D](steps: Steps[D]) extends SequenceGenerator[D]:

      override def generate(ignore: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
        gen(steps.initialAtom.some, steps.slit, IndexTracker.Zero)

      override def record(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
        ExpectCcd2(step.visitId, IndexTracker.Zero, steps).record(step)

    end Init

    case class ExpectCcd2[D](visitId: Visit.Id, tracker: IndexTracker, steps: Steps[D]) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
        gen(steps.initialAtom.some, steps.slit, tracker)

      override def updateTracker(tracker: IndexTracker): AcquisitionState[D] =
        copy(tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.ccd2.matches(step) then ExpectP10(visitId, tracker, steps)
        else this

    end ExpectCcd2

    case class ExpectP10[D](visitId: Visit.Id, tracker: IndexTracker, steps: Steps[D]) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
        gen(NonEmptyList.of(steps.p10, steps.slit).some, steps.slit, tracker)

      override def updateTracker(tracker: IndexTracker): AcquisitionState[D] =
        copy(tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.p10.matches(step) then ExpectSlit(visitId, tracker, steps, initialAtom = true)
        else this

    end ExpectP10

    case class ExpectSlit[D](visitId: Visit.Id, tracker: IndexTracker, steps: Steps[D], initialAtom: Boolean) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, (ProtoAtom[(ProtoStep[D], Int)], Int)] =
        gen(Option.when(initialAtom)(NonEmptyList.one(steps.slit)), steps.slit, tracker)

      override def updateTracker(tracker: IndexTracker): AcquisitionState[D] =
        copy(tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.slit.matches(step) then ExpectSlit(visitId, tracker, steps, initialAtom = false)
        else this

    end ExpectSlit

  end AcquisitionState

  def gmosNorth(config: Config.GmosNorth, exposureTime: TimeSpan): SequenceGenerator[GmosNorth] =
    AcquisitionState.Init(
      StepComputer.North.compute(GmosNorthFilter.acquisition, config.fpu, exposureTime, config.centralWavelength)
    )

  def gmosSouth(config: Config.GmosSouth, exposureTime: TimeSpan): SequenceGenerator[GmosSouth] =
    AcquisitionState.Init(
      StepComputer.South.compute(GmosSouthFilter.acquisition, config.fpu, exposureTime, config.centralWavelength)
    )

end Acquisition