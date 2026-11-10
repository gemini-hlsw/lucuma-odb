// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package mos

import cats.data.NonEmptyList
import cats.syntax.either.*
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
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2AcquisitionMode
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.TimeSpan
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
 * confirm the targets land in the slitlets. A Faint acquisition, the default,
 * needs a sky frame to subtract, taken before the on-target step with the mask
 * out and again with it in; a Bright one takes no sky steps.
 *
 * The "Fine Adjustments" atoms that follow are a ceiling rather than a plan:
 * each carries a breakpoint, so taking another through-mask image is always the
 * observer's explicit decision. They reuse the sky frame of the initial atom.
 */
object Acquisition:

  /** Number of optional "Fine Adjustments" atoms generated after the initial atom. */
  val RepeatingAtomCount: Int = 10

  /** The sky steps are present only in a Faint acquisition. */
  case class Steps(
    maskOut:    ProtoStep[F2],
    maskOutSky: Option[ProtoStep[F2]],
    maskIn:     ProtoStep[F2],
    maskInSky:  Option[ProtoStep[F2]]
  ):
    /**
     * The breakpoint sits on the first through-mask step: execution halts there
     * so the observer can measure and apply the offset from the field image.
     */
    val initialAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.ofInitLast(
        maskOutSky.toList ++ List(maskOut) ++ maskInSky.map(_.withBreakpoint).toList,
        maskInSky.fold(maskIn.withBreakpoint)(_ => maskIn)
      )

    /** An optional extra through-mask image, gated by a breakpoint. */
    val repeatingAtom: NonEmptyList[ProtoStep[F2]] =
      NonEmptyList.one(maskIn.withBreakpoint)

  private object StepComputer extends SequenceState[F2] with Flamingos2InitialDynamicConfig:
    def compute(
      exposureTime: TimeSpan,
      acqConfig:    AcquisitionConfig,
      mode:         Flamingos2AcquisitionMode,
      customMask:   Flamingos2FpuMask.Custom
    ): Steps =
      val skyOffset: Option[Offset] =
        Flamingos2AcquisitionMode.skyOffset.getOption(mode)

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
          s0 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          s1 <- skyOffset.traverse(scienceStep(_, ObserveClass.Acquisition))

          // Through the mask, which the science steps carry whether or not its
          // attachment exists yet.
          _  <- F2.fpu         := customMask
          _  <- F2.decker      := customMask.defaultDecker
          s2 <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          s3 <- skyOffset.traverse(scienceStep(_, ObserveClass.Acquisition))
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

  private def exposureTime(
    observationId: Observation.Id,
    etm:           ExposureTimeMode
  ): Either[OdbError, TimeSpan] =
    etm match
      case ExposureTimeMode.TimeAndCountMode(t, _, _) => t.asRight
      case _                                          =>
        OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: ${ObservingMode.Flamingos2MosName} acquisition requires a Time & Count exposure time mode.".some).asLeft

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[Flamingos2StaticConfig, F2],
    static:        Flamingos2StaticConfig,
    namespace:     UUID,
    config:        Config
  ): Either[OdbError, SequenceGenerator[F2]] =
    exposureTime(observationId, config.acquisition.exposureTimeMode)
      .filterOrElse(
        _.toNonNegMicroseconds.value > 0,
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
          StepComputer.compute(
            t,
            config.acquisition,
            // The brightness of the mask's acquisition stars is unknown, so the sky
            // steps are taken unless the user asks for a Bright acquisition.
            config.acquisition.explicitMode.getOrElse(Flamingos2AcquisitionMode.Faint.Default),
            config.customMask
          )
        )
