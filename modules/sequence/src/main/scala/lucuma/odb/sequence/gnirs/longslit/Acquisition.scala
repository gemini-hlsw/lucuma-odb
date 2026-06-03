// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package longslit

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * GNIRS long slit acquisition sequence generation.
 */
object Acquisition:

  val RepeatingAtomCount: Int = 10

  case class Steps(
    slitImage:      ProtoStep[GnirsDynamicConfig],
    field:          ProtoStep[GnirsDynamicConfig],
    fieldSky:       Option[ProtoStep[GnirsDynamicConfig]],
    throughSlit:    ProtoStep[GnirsDynamicConfig],
    throughSlitSky: Option[ProtoStep[GnirsDynamicConfig]]
  ):
    private val hasSky = fieldSky.isDefined

    val initialAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      if hasSky then
        NonEmptyList.of(
          slitImage,
          fieldSky.get,
          field,
          throughSlitSky.get,
          throughSlit.withBreakpoint
        )
      else
        NonEmptyList.of(slitImage, field, throughSlit.withBreakpoint)

    val repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      NonEmptyList.of(throughSlit)

  private object StepComputer extends GnirsSequenceState:

    def compute(config: Config, mode: GnirsAcquisitionMode, selectedFilter: GnirsFilter, time: IntegrationTime): Steps =
      val acqExposureTime = time.exposureTime
      val readMode        = GnirsReadMode.forExposureTime(acqExposureTime)
      val slitDecker      = GnirsDecker.forCameraAndReadMode(config.camera, GnirsPrism.Mirror)

      // The FPU image (first step) always uses H (Order4) for VeryBright; for Bright
      // and Faint it uses the selected filter. All other steps use the selected
      // filter. Sky frames are generated only for Faint, at its sky offset.
      val fpuStepFilter: GnirsFilter =
        mode match
          case GnirsAcquisitionMode.VeryBright => GnirsFilter.Order4
          case _                               => selectedFilter
      val skyOffsetOpt: Option[Offset] = GnirsAcquisitionMode.skyOffset.getOption(mode)

      eval:
        for
          _ <- State.modify[GnirsDynamicConfig]: dyn =>
                 dyn.copy(
                   exposure          = acqExposureTime,
                   coadds            = config.acquisition.coadds,
                   filter            = fpuStepFilter,
                   acquisitionMirror = GnirsAcquisitionMirrorMode.In,
                   camera            = config.camera,
                   focus             = config.focus,
                   readMode          = readMode,
                   decker            = slitDecker,
                   fpu               = Left(config.fpu)
                 )
          slitImage      <- scienceStep(
                              TelescopeConfig(
                                Offset(Offset.P(10.arcsec), Offset.Q(0.arcsec)),
                                Disabled
                              ),
                              ObserveClass.Acquisition
                            )
          // Subsequent steps switch to the acquisition decker/FPU and the selected filter.
          _              <- State.modify[GnirsDynamicConfig]:
                              _.copy(decker = GnirsDecker.Acquisition, fpu = Right(GnirsFpuOther.Acquisition), filter = selectedFilter)
          fieldSkyOpt    <- skyOffsetOpt.traverse: sky =>
                              scienceStep(TelescopeConfig(sky, Enabled), ObserveClass.Acquisition)
          field          <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          // Back to the selected slit (decker/FPU) for the through-slit steps.
          _              <- State.modify[GnirsDynamicConfig]:
                              _.copy(decker = slitDecker, fpu = Left(config.fpu))
          tSlitSkyOpt    <- skyOffsetOpt.traverse: sky =>
                              scienceStep(TelescopeConfig(sky, Enabled), ObserveClass.Acquisition)
          throughSlit    <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Steps(
          slitImage      = slitImage,
          field          = field,
          fieldSky       = fieldSkyOpt,
          throughSlit    = throughSlit,
          throughSlitSky = tSlitSkyOpt
        )

  private class Generator(
    builder: AtomBuilder[GnirsDynamicConfig],
    steps:   Steps
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    override val generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
      (for
        a0 <- builder.build(NonEmptyString.unapply("Initial Acquisition"), 0, 0, steps.initialAtom)
        as <- (1 to RepeatingAtomCount).toList.traverse: aix =>
                builder.build(NonEmptyString.unapply("Fine Adjustments"), aix, 0, steps.repeatingAtom)
      yield Stream.emits(a0 :: as)).runA(StepTimeEstimateCalculator.Last.empty[GnirsDynamicConfig]).value

  def instantiate(
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): Either[OdbError, SequenceGenerator[GnirsDynamicConfig]] =
    for
      t <- time.filterOrElse(
             _.exposureTime.toNonNegMicroseconds.value > 0,
             OdbError.SequenceUnavailable(
               observationId,
               s"Could not generate a sequence for $observationId: GNIRS Long Slit requires a positive acquisition exposure time.".some
             )
           )
      // Resolve the acquisition mode (explicit or auto) and the selected filter
      // (explicit, or auto from the mode + spectroscopy wavelength).
      mode       = config.acquisition.resolvedMode(t.exposureTime)
      selFilter <- config.acquisition
                     .selectedFilter(mode, config.gratingWavelength)
                     .leftMap: msg =>
                       OdbError.SequenceUnavailable(
                         observationId,
                         s"Could not generate a sequence for $observationId: $msg".some
                       )
    yield
      new Generator(
        AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition),
        StepComputer.compute(config, mode, selFilter, t)
      )
