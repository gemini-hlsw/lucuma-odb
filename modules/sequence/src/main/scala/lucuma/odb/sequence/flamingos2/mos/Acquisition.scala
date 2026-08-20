// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package mos

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Flamingos 2 MOS acquisition.
 *
 * The observer cannot measure the offset through the mask, so the initial atom
 * images the field with the mask out of the beam and then, after a breakpoint
 * where the measured offset is applied, repeats the pair through the mask to
 * confirm the targets land in the slitlets.  Each pair is an on-target step and
 * its sky nod.
 *
 * The "Fine Adjustments" atoms that follow are a ceiling rather than a plan:
 * each carries a breakpoint, so taking another mask-in pair is always the
 * observer's explicit decision.
 */
object Acquisition:

  /** Number of optional "Fine Adjustments" atoms generated after the initial atom. */
  val RepeatingAtomCount: Int = 10

  case class Steps(
    maskOut:   ProtoStep[F2],
    maskOutQ:  ProtoStep[F2],
    maskIn:    ProtoStep[F2],
    maskInQ:   ProtoStep[F2]
  ):
    /**
     * Mask out, its sky nod, then the through-mask pair.  The breakpoint sits on
     * the first mask-in step: execution halts there so the observer can measure
     * and apply the offset from the two field images.
     */
    val initialAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(maskOut, maskOutQ, maskIn.withBreakpoint, maskInQ)

    /** An optional extra through-mask pair, gated by a breakpoint. */
    val repeatingAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.of(maskIn.withBreakpoint, maskInQ)

  private object StepComputer extends SequenceState[F2] with Flamingos2InitialDynamicConfig:
    def compute(
      exposureTime: TimeSpan,
      acqConfig:    AcquisitionConfig,
      customMask:   Flamingos2FpuMask.Custom
    ): Steps =
      eval:
        for
          _  <- F2.exposure    := exposureTime
          _  <- F2.disperser   := none[Flamingos2Disperser]
          _  <- F2.filter      := acqConfig.filter
          _  <- F2.readMode    := Flamingos2ReadMode.forExposureTime(exposureTime)
          _  <- F2.lyotWheel   := Flamingos2LyotWheel.F16
          _  <- F2.readoutMode := Flamingos2ReadoutMode.Science
          _  <- F2.reads       := Flamingos2ReadMode.forExposureTime(exposureTime).readCount

          // The mask is out of the beam: this is plain imaging of the field.
          _  <- F2.fpu         := Flamingos2FpuMask.Imaging
          _  <- F2.decker      := Flamingos2FpuMask.Imaging.defaultDecker
          s0 <- scienceStep(0.arcsec,  0.arcsec, ObserveClass.Acquisition)
          s1 <- scienceStep(0.arcsec, 10.arcsec, ObserveClass.Acquisition)

          // Through the mask, which the science steps carry whether or not its
          // attachment exists yet.
          _  <- F2.fpu         := customMask
          _  <- F2.decker      := customMask.defaultDecker
          s2 <- scienceStep(0.arcsec,  0.arcsec, ObserveClass.Acquisition)
          s3 <- scienceStep(0.arcsec, 10.arcsec, ObserveClass.Acquisition)
        yield Steps(s0, s1, s2, s3)

  private class Generator(
    builder: AtomBuilder[F2],
    steps:   Steps
  ) extends SequenceGenerator[F2]:

    override val generate: Stream[Pure, Atom[F2]] =
      (for
        a0 <- builder.build(NonEmptyString.unapply("Initial Acquisition"), 0, 0, steps.initialAtom)
        as <- (1 to RepeatingAtomCount).toList.traverse: aix =>
                builder.build(NonEmptyString.unapply("Fine Adjustments"), aix, 0, steps.repeatingAtom)
      yield Stream.emits(a0 :: as)).runA(StepTimeEstimateCalculator.Last.empty[F2]).value

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): Either[OdbError, SequenceGenerator[F2]] =
    time
      .filterOrElse(
        _.exposureTime.toNonNegMicroseconds.value > 0,
        OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: Flamingos 2 MOS requires a positive exposure time.".some)
      )
      .map: t =>
        new Generator(
          AtomBuilder.instantiate(
            estimator,
            static,
            namespace,
            SequenceType.Acquisition
          ),
          StepComputer.compute(t.exposureTime, config.acquisition, config.customMask)
        )
