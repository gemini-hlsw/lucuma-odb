// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
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
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Visit
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
import lucuma.itc.TargetIntegrationTime
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.odb.sequence.util.IndexTracker
import lucuma.refined.*

import java.util.UUID

object Acquisition:
  val AcquisitionSN: SignalToNoise =
    SignalToNoise.FromBigDecimalExact.getOption(10).get

  val DefaultIntegrationTime: TargetIntegrationTime =
    TargetIntegrationTime(
      Zipper.one(IntegrationTime(TimeSpan.fromSeconds(1).get, 1.refined, AcquisitionSN)),
      Band.R.asLeft // Band is meaningless here, but we need to provide one
    )

  val MinExposureTime    =   1.secondTimeSpan
  val MaxExposureTime    = 180.secondTimeSpan
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

    def builder: AtomBuilder[D]
    def visitId: Visit.Id
    def steps: Steps[D]
    def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D]
    def calcState: TimeEstimateCalculator.Last[D]
    def tracker: IndexTracker

    def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D]

    def updatesVisit(step: StepRecord[D]): Boolean =
      step.visitId =!= visitId

    def reset(visitId: Visit.Id): AcquisitionState[D] =
      AcquisitionState.ExpectCcd2(visitId, calcState, tracker, builder, steps)

    override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      if updatesVisit(step) then
        reset(step.visitId).recordStep(step)
      else
        val a = updateTracker(calcState.next(step.protoStep), tracker.record(step))
        if !step.isAcquisitionSequence     then a.reset(step.visitId)
        else if step.successfullyCompleted then a.recordCompleted(step)
        else a

    // when a new visit is recorded, we reset the acquisition so that it begins
    // at the first step.
    override def recordVisit(visit: VisitRecord): SequenceGenerator[D] =
      if visit.visitId === visitId then this
      else reset(visit.visitId)

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

    case class Init[D](builder: AtomBuilder[D], steps: Steps[D]) extends SequenceGenerator[D]:

      override def generate(ignore: Timestamp): Stream[Pure, Atom[D]] =
        gen(builder, steps.initialAtom.some, steps.slit, TimeEstimateCalculator.Last.empty[D], IndexTracker.Zero)

      override def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
        ExpectCcd2(step.visitId, TimeEstimateCalculator.Last.empty[D], IndexTracker.Zero, builder, steps).recordStep(step)

      override def recordVisit(visit: VisitRecord): SequenceGenerator[D] =
        this

    end Init

    case class ExpectCcd2[D](visitId: Visit.Id, calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D]) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, Atom[D]] =
        gen(builder, steps.initialAtom.some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.ccd2.matches(step) then ExpectP10(visitId, calcState, tracker, builder, steps)
        else this

    end ExpectCcd2

    case class ExpectP10[D](visitId: Visit.Id, calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D]) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, Atom[D]] =
        gen(builder, NonEmptyList.of(steps.p10, steps.slit).some, steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.p10.matches(step) then ExpectSlit(visitId, calcState, tracker, builder, steps, initialAtom = true)
        else this

    end ExpectP10

    case class ExpectSlit[D](visitId: Visit.Id, calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker, builder: AtomBuilder[D], steps: Steps[D], initialAtom: Boolean) extends AcquisitionState[D]:

      override def generate(ignore: Timestamp): Stream[Pure, Atom[D]] =
        gen(builder, Option.when(initialAtom)(NonEmptyList.one(steps.slit)), steps.slit, calcState, tracker)

      override def updateTracker(calcState: TimeEstimateCalculator.Last[D], tracker: IndexTracker): AcquisitionState[D] =
        copy(calcState = calcState, tracker = tracker)

      override def recordCompleted(step: StepRecord[D])(using Eq[D]): AcquisitionState[D] =
        if steps.slit.matches(step) then ExpectSlit(visitId, calcState, tracker, builder, steps, initialAtom = false)
        else this

    end ExpectSlit


  end AcquisitionState

  private def instantiate[D, G, L, U](
    stepComp:    StepComputer[D, G, L, U],
    time:        Either[MissingParamSet, IntegrationTime],
    calRole:     Option[CalibrationRole],
    atomBuilder: AtomBuilder[D],
    acqFilters:  NonEmptyList[L],
    fpu:         U,
    λ:           Wavelength
  ): Either[String, SequenceGenerator[D]] =
    calRole match
      case Some(CalibrationRole.Twilight) =>
        SequenceGenerator.empty.asRight

      case _                              =>
        time
          .bimap(
            m => s"GMOS Long Slit acquisition requires a valid target: ${m.format}",
            t => AcquisitionState.Init(atomBuilder, stepComp.compute(acqFilters, fpu, t.exposureTime, λ))
          )

  def gmosNorth(
    estimator: TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:    StaticConfig.GmosNorth,
    namespace: UUID,
    config:    Config.GmosNorth,
    time:      Either[MissingParamSet, IntegrationTime],
    calRole:   Option[CalibrationRole]
  ): Either[String, SequenceGenerator[GmosNorth]] =
    instantiate(
      StepComputer.North,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      GmosNorthFilter.acquisition,
      config.fpu,
      config.centralWavelength
    )

  def gmosSouth(
    estimator: TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:    StaticConfig.GmosSouth,
    namespace: UUID,
    config:    Config.GmosSouth,
    time:      Either[MissingParamSet, IntegrationTime],
    calRole:   Option[CalibrationRole]
  ): Either[String, SequenceGenerator[GmosSouth]] =
    instantiate(
      StepComputer.South,
      time,
      calRole,
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
      GmosSouthFilter.acquisition,
      config.fpu,
      config.centralWavelength
    )

end Acquisition