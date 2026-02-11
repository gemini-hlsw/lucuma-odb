// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
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
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Flamingos 2 long slit acquisition.
 */
object Acquisition:

  val SkySubtractionLimit: TimeSpan = 2.secondTimeSpan

  extension (scienceFilter: Flamingos2Filter)
    def toAcquisitionFilter: Flamingos2Filter =
      Flamingos2Filter.acquisition.toList.minBy(f => scienceFilter.wavelength.diff(f.wavelength).abs)

  case class Steps(
    image:    ProtoStep[F2],
    imageQ10: ProtoStep[F2],
    slitP10:  ProtoStep[F2],
    slitQ10:  ProtoStep[F2],
    slit:     ProtoStep[F2]
  ):
    val initialAtom: NonEmptyList[ProtoStep[F2]] =
      if image.value.exposure <= SkySubtractionLimit then
        NonEmptyList.of(image, slitP10, slit.withBreakpoint)
      else
        NonEmptyList.of(imageQ10, image, slitP10, slitQ10, slit.withBreakpoint)

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
          s0 <- scienceStep(0.arcsec,  0.arcsec, ObserveClass.Acquisition)
          s1 <- scienceStep(0.arcsec, 10.arcsec, ObserveClass.Acquisition)

          _  <- F2.exposure    := 10.secondTimeSpan  // Fixed
          _  <- F2.fpu         := Flamingos2FpuMask.Builtin(builtin)
          _  <- F2.decker      := Flamingos2FpuMask.Builtin(builtin).defaultDecker
          s2 <- scienceStep(10.arcsec,  0.arcsec, ObserveClass.Acquisition)

          _  <- F2.exposure    := exposureTime
          s3 <- scienceStep( 0.arcsec, 10.arcsec, ObserveClass.Acquisition)

          s4 <- scienceStep( 0.arcsec,  0.arcsec, ObserveClass.Acquisition)
        yield Steps(s0, s1, s2, s3, s4)

  private class Generator(
    builder:   AtomBuilder[F2],
    steps:     Steps
  ) extends SequenceGenerator[F2]:

    override val generate: Stream[Pure, Atom[F2]] =
      (for
        a0 <- builder.build(NonEmptyString.unapply("Initial Acquisition"), 0, 0, steps.initialAtom)
        a1 <- builder.build(NonEmptyString.unapply("Fine Adjustments"),    1, 0, steps.repeatingAtom)
      yield Stream(a0, a1)).runA(TimeEstimateCalculator.Last.empty[F2]).value

  def instantiate(
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): Either[OdbError, SequenceGenerator[F2]] =
    time
      .filterOrElse(
        _.exposureTime.toNonNegMicroseconds.value > 0,
        OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: Flamingos 2 Long Slit requires a positive exposure time.".some)
      )
      .map: t =>
        new Generator(
          AtomBuilder.instantiate(
            estimator,
            static,
            namespace,
            SequenceType.Acquisition
          ),
          StepComputer.compute(t.exposureTime, config.filter, config.fpu)
        )