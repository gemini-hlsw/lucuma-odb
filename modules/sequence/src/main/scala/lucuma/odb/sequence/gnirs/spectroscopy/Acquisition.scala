// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package spectroscopy

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsPixelScale
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
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.refined.*

import java.util.UUID

/**
 * GNIRS spectroscopy (long slit and IFU) acquisition sequence generation.
 */
object Acquisition:

  val RepeatingAtomCount: Int = 10

  /**
   * The offset IFU image (Very Bright / Bright) is a fixed short single-coadd
   * exposure, always in H (Order4).
   */
  val IfuImageExposureTime: TimeSpan = 6.secTimeSpan
  val IfuImageFilter: GnirsFilter    = GnirsFilter.Order4

  private val SingleCoadd: PosInt = 1.refined

  /** The default Faint-mode sky offset when the acquisition mode is auto-resolved. */
  private def defaultFaintSkyOffset(fpu: GnirsFpu.Spectroscopy): Offset =
    fpu match
      case GnirsFpu.Spectroscopy.Slit(_) => GnirsAcquisitionMode.Faint.DefaultSlitSkyOffset
      case GnirsFpu.Spectroscopy.Ifu(_)  => GnirsAcquisitionMode.Faint.DefaultIfuSkyOffset

  /**
   * The filter and (fixed, single-coadd) exposure time for the FPU image — the first
   * acquisition step — as a function of the acquisition mode, the camera (short =
   * 0.15"/pix, long = 0.05"/pix) and the selected acquisition filter.
   *
   * PAH can never be used on the short camera (the sky is too bright), regardless of
   * mode — that yields an error. Otherwise VeryBright always images the FPU in H
   * (Order4), and for Bright/Faint the values come from a per-camera table (the X/J/H/K
   * bands are the spectroscopy order filters Order6/Order5/Order4/Order3 that auto
   * selection produces):
   *
   *   Short:  X=10s, J=15s, H=3s, K=3s, H2→H(3s), PAH→error (sky too bright)
   *   Long:   X→H, J→H, H=15s, K=15s, H2→H(15s), PAH=0.5s
   *
   * See https://app.shortcut.com/lucuma/story/8880/gnirs-acquisition-initial-slit-image
   *
   * Any other filter (e.g. a user-selected filter) falls back to H.
   */
  private def firstStepFilterAndExposure(
    mode:           GnirsAcquisitionMode,
    camera:         GnirsCamera,
    selectedFilter: GnirsFilter
  ): Either[String, (GnirsFilter, TimeSpan)] =
    // "Use H": image the FPU in H (Order4) at the camera's H exposure (short 3s, long 15s).
    val useH: (GnirsFilter, TimeSpan) =
      (GnirsFilter.Order4, if camera.pixelScale === GnirsPixelScale.PixelScale_0_05 then 15.secTimeSpan else 3.secTimeSpan)
    (mode, selectedFilter, camera.pixelScale) match
      case (_, GnirsFilter.PAH, GnirsPixelScale.PixelScale_0_15)    =>
        s"PAH acquisition filter cannot be used with short camera".asLeft
      case (GnirsAcquisitionMode.VeryBright, _, _)                  => useH.asRight
      case (_, GnirsFilter.Order6, GnirsPixelScale.PixelScale_0_15) => (GnirsFilter.Order6, 10.secTimeSpan).asRight // X, short
      case (_, GnirsFilter.Order5, GnirsPixelScale.PixelScale_0_15) => (GnirsFilter.Order5, 15.secTimeSpan).asRight // J, short
      case (_, GnirsFilter.Order3, GnirsPixelScale.PixelScale_0_15) => (GnirsFilter.Order3,  3.secTimeSpan).asRight // K, short
      case (_, GnirsFilter.Order3, GnirsPixelScale.PixelScale_0_05) => (GnirsFilter.Order3, 15.secTimeSpan).asRight // K, long
      case (_, GnirsFilter.PAH,    GnirsPixelScale.PixelScale_0_05) => (GnirsFilter.PAH,    500.msTimeSpan).asRight // PAH, long
      case _                                                        => useH.asRight // H, H2, long-camera X/J, L/M orders, broadband J/K, …

  /** A calibration step's read mode is determined by its (SmartGcal-provided) exposure time. */
  private def adjustReadMode(s: ProtoStep[GnirsDynamicConfig]): ProtoStep[GnirsDynamicConfig] =
    s.copy(value = s.value.copy(readMode = GnirsReadMode.forExposureTime(s.value.exposure)))

  case class Steps(
    slitImage:      ProtoStep[GnirsDynamicConfig],
    field:          ProtoStep[GnirsDynamicConfig],
    fieldSky:       Option[ProtoStep[GnirsDynamicConfig]],
    throughSlit:    ProtoStep[GnirsDynamicConfig],
    throughSlitSky: Option[ProtoStep[GnirsDynamicConfig]]
  ):
    private val hasSky = fieldSky.isDefined

    // The field image is taken twice.
    val initialAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      if hasSky then
        NonEmptyList.of(
          slitImage,
          fieldSky.get,
          field,
          field,
          throughSlitSky.get,
          throughSlit.withBreakpoint
        )
      else
        NonEmptyList.of(slitImage, field, field, throughSlit.withBreakpoint)

    val repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      NonEmptyList.of(throughSlit)

  case class IfuSteps(
    flats:      List[ProtoStep[GnirsDynamicConfig]],
    fieldSky:   Option[ProtoStep[GnirsDynamicConfig]],
    field:      ProtoStep[GnirsDynamicConfig],
    ifuImage:   Option[ProtoStep[GnirsDynamicConfig]],
    throughSky: Option[ProtoStep[GnirsDynamicConfig]],
    through:    ProtoStep[GnirsDynamicConfig]
  ):
    val initialAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      // The through-IFU phase pauses first for the operator to apply the offsets
      // measured on the field image, so its first step carries a breakpoint.
      val throughPhase: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
        NonEmptyList.fromListUnsafe(ifuImage.toList ++ throughSky.toList ++ List(through, through))
      NonEmptyList.fromListUnsafe(
        flats ++ fieldSky.toList ++ (field :: throughPhase.head.withBreakpoint :: throughPhase.tail)
      )

    val repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      NonEmptyList.of(through)

  private object StepComputer extends GnirsSequenceState:

    def compute(
      config:              Config,
      mode:                GnirsAcquisitionMode,
      fpuStepFilter:       GnirsFilter,
      fpuStepExposureTime: TimeSpan,
      selectedFilter:      GnirsFilter,
      time:                IntegrationTime
    ): Steps =
      val acqExposureTime: TimeSpan = time.exposureTime
      val readMode: GnirsReadMode   = GnirsReadMode.forExposureTime(acqExposureTime)
      // The science-aperture decker, from camera + prism (always Mirror) for the long
      // slit.  (IFU acquisitions take the computeIfu path.)
      val specDecker: GnirsDecker   = GnirsDecker.forCameraAndPrism(config.camera, GnirsPrism.Mirror)

      // The FPU image (first step) uses a single coadd and the fixed filter/exposure from
      // firstStepFilterAndExposure; its read mode follows from that fixed exposure time.
      // The remaining steps use the selected filter and the ITC exposure. Only the
      // acquisition-FPU/decker field steps use the resolved coadds (the ITC exposure count
      // in S/N mode, else the explicit acquisition coadds); the through-slit steps keep the
      // explicit acquisition coadds. Sky frames are generated only for Faint, at its sky
      // offset.
      val fpuStepReadMode: GnirsReadMode = GnirsReadMode.forExposureTime(fpuStepExposureTime)
      val fieldCoadds: PosInt            = config.acquisition.resolvedCoadds(time)

      val skyOffsetOpt: Option[Offset] = GnirsAcquisitionMode.skyOffset.getOption(mode)

      eval:
        for
          _ <- State.modify[GnirsDynamicConfig]: dyn =>
                 dyn.copy(
                   exposure          = fpuStepExposureTime,
                   coadds            = SingleCoadd,
                   filter            = fpuStepFilter,
                   acquisitionMirror = GnirsAcquisitionMirrorMode.In,
                   camera            = config.camera,
                   focus             = config.focus,
                   readMode          = fpuStepReadMode,
                   decker            = specDecker,
                   fpu               = config.fpu
                 )
          slitImage      <- scienceStep(
                              TelescopeConfig(
                                Offset(Offset.P(10.arcsec), Offset.Q(0.arcsec)),
                                Disabled
                              ),
                              ObserveClass.Acquisition
                            )
          // The field steps switch to the acquisition decker/FPU and the selected filter,
          // and use the ITC exposure time, read mode and field coadds.
          _              <- State.modify[GnirsDynamicConfig]:
                              _.copy(
                                exposure = acqExposureTime,
                                coadds   = fieldCoadds,
                                readMode = readMode,
                                decker   = GnirsDecker.Acquisition,
                                fpu      = GnirsFpu.Other(GnirsFpuOther.Acquisition),
                                filter   = selectedFilter
                              )
          fieldSkyOpt    <- skyOffsetOpt.traverse: sky =>
                              scienceStep(TelescopeConfig(sky, Enabled), ObserveClass.Acquisition)
          field          <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          // Back to the science aperture (decker/FPU) for the through-slit steps, which revert
          // to the explicit acquisition coadds (the ITC count is used only for the field).
          _              <- State.modify[GnirsDynamicConfig]:
                              _.copy(decker = specDecker, fpu = config.fpu, coadds = config.acquisition.coadds)
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

    /**
     * The unexpanded smart flat leading an HR-IFU acquisition, taken at the science
     * configuration to check the alignment of the spectrograph. Exposure, lamp and
     * coadds come from the SmartGcal lookup.
     */
    def ifuAlignmentFlat(config: Config): ProtoStep[GnirsDynamicConfig] =
      eval:
        for
          _ <- State.modify[GnirsDynamicConfig]: dyn =>
                 dyn.copy(
                   coadds            = config.coadds,
                   filter            = config.filter,
                   decker            = config.decker,
                   fpu               = config.fpu,
                   acquisitionMirror = GnirsAcquisitionMirrorMode.Out(
                                         config.prism,
                                         config.grating,
                                         GnirsGratingWavelength(config.centralWavelength)
                                       ),
                   camera            = config.camera,
                   focus             = config.focus
                 )
          f <- flatStep(TelescopeConfig(Offset.Zero, Disabled), ObserveClass.Acquisition)
        yield f

    def computeIfu(
      config:         Config,
      ifu:            GnirsFpuIfu,
      mode:           GnirsAcquisitionMode,
      selectedFilter: GnirsFilter,
      flats:          List[ProtoStep[GnirsDynamicConfig]],
      time:           IntegrationTime
    ): IfuSteps =
      val fieldExposureTime: TimeSpan   = time.exposureTime
      // The through-IFU steps expose twice as long as the field image (per the OCS IFU
      // acquisition templates).
      val throughExposureTime: TimeSpan = fieldExposureTime *| 2
      val ifuDecker: GnirsDecker        = GnirsDecker.forIfu(ifu)
      val fieldCoadds: PosInt           = config.acquisition.resolvedCoadds(time)
      val skyOffsetOpt: Option[Offset]  = GnirsAcquisitionMode.skyOffset.getOption(mode)

      eval:
        for
          // Field steps: acquisition decker/FPU, selected filter, ITC exposure and
          // field coadds.  Faint takes a sky frame first, at its sky offset.
          _           <- State.modify[GnirsDynamicConfig]: dyn =>
                           dyn.copy(
                             exposure          = fieldExposureTime,
                             coadds            = fieldCoadds,
                             filter            = selectedFilter,
                             acquisitionMirror = GnirsAcquisitionMirrorMode.In,
                             camera            = config.camera,
                             focus             = config.focus,
                             readMode          = GnirsReadMode.forExposureTime(fieldExposureTime),
                             decker            = GnirsDecker.Acquisition,
                             fpu               = GnirsFpu.Other(GnirsFpuOther.Acquisition)
                           )
          fieldSkyOpt <- skyOffsetOpt.traverse: sky =>
                           scienceStep(TelescopeConfig(sky, Enabled), ObserveClass.Acquisition)
          field       <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
          // Very Bright / Bright image the sky through the IFU with a fixed short
          // single-coadd exposure in H; Faint takes a full through-IFU sky instead.
          ifuImageOpt <- Option.when(skyOffsetOpt.isEmpty)(()).traverse: _ =>
                           for
                             _ <- State.modify[GnirsDynamicConfig]:
                                    _.copy(
                                      exposure = IfuImageExposureTime,
                                      coadds   = SingleCoadd,
                                      filter   = IfuImageFilter,
                                      readMode = GnirsReadMode.forExposureTime(IfuImageExposureTime),
                                      decker   = ifuDecker,
                                      fpu      = config.fpu
                                    )
                             s <- scienceStep(TelescopeConfig(GnirsAcquisitionMode.Faint.DefaultIfuSkyOffset, Enabled), ObserveClass.Acquisition)
                           yield s
          // Through-IFU steps: back to the IFU decker/FPU and the explicit acquisition
          // coadds (the ITC count is used only for the field).
          _           <- State.modify[GnirsDynamicConfig]:
                           _.copy(
                             exposure = throughExposureTime,
                             coadds   = config.acquisition.coadds,
                             filter   = selectedFilter,
                             readMode = GnirsReadMode.forExposureTime(throughExposureTime),
                             decker   = ifuDecker,
                             fpu      = config.fpu
                           )
          tSkyOpt     <- skyOffsetOpt.traverse: sky =>
                           scienceStep(TelescopeConfig(sky, Enabled), ObserveClass.Acquisition)
          through     <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield IfuSteps(
          flats      = flats,
          fieldSky   = fieldSkyOpt,
          field      = field,
          ifuImage   = ifuImageOpt,
          throughSky = tSkyOpt,
          through    = through
        )

  private class Generator(
    builder:       AtomBuilder[GnirsDynamicConfig],
    initialAtom:   NonEmptyList[ProtoStep[GnirsDynamicConfig]],
    repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]]
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    override val generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
      (for
        a0 <- builder.build(NonEmptyString.unapply("Initial Acquisition"), 0, 0, initialAtom)
        as <- (1 to RepeatingAtomCount).toList.traverse: aix =>
                builder.build(NonEmptyString.unapply("Fine Adjustments"), aix, 0, repeatingAtom)
      yield Stream.emits(a0 :: as)).runA(StepTimeEstimateCalculator.Last.empty[GnirsDynamicConfig]).value

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    time:          Either[OdbError, IntegrationTime]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    def sequenceError(msg: String): OdbError =
      OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: $msg".some)

    val builder: AtomBuilder[GnirsDynamicConfig] =
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition)

    // Resolve the acquisition mode (explicit or auto) and the selected filter (explicit,
    // or auto from the mode + spectroscopy wavelength).
    val checked: Either[OdbError, (IntegrationTime, GnirsAcquisitionMode, GnirsFilter)] =
      for
        t         <- time.filterOrElse(
                       _.exposureTime.toNonNegMicroseconds.value > 0,
                       sequenceError("GNIRS Spectroscopy requires a positive acquisition exposure time.")
                     )
        mode       = config.acquisition.resolvedMode(t, defaultFaintSkyOffset(config.fpu))
        selFilter <- config.acquisition
                       .selectedFilter(mode, config.centralWavelength)
                       .leftMap(sequenceError)
      yield (t, mode, selFilter)

    config.fpu match
      case GnirsFpu.Spectroscopy.Slit(_)  =>
        (for
          (t, mode, selFilter) <- checked
          fpuStep              <- firstStepFilterAndExposure(mode, config.camera, selFilter).leftMap(sequenceError)
        yield
          val (fpuStepFilter, fpuStepExposureTime): (GnirsFilter, TimeSpan) = fpuStep
          val steps: Steps = StepComputer.compute(config, mode, fpuStepFilter, fpuStepExposureTime, selFilter, t)
          Generator(builder, steps.initialAtom, steps.repeatingAtom): SequenceGenerator[GnirsDynamicConfig]
        ).pure[F]

      case GnirsFpu.Spectroscopy.Ifu(ifu) =>
        // HR-IFU acquisitions lead with a smart flat that checks the alignment of the
        // spectrograph.
        val flats: EitherT[F, OdbError, List[ProtoStep[GnirsDynamicConfig]]] =
          ifu match
            case GnirsFpuIfu.LowResolution  => EitherT.pure(List.empty)
            case GnirsFpuIfu.HighResolution =>
              EitherT(expander.expandStep(static, StepComputer.ifuAlignmentFlat(config)))
                .bimap(sequenceError, _.toList.map(adjustReadMode))

        (for
          (t, mode, selFilter) <- EitherT.fromEither[F](checked)
          fs                   <- flats
        yield
          val steps: IfuSteps = StepComputer.computeIfu(config, ifu, mode, selFilter, fs, t)
          Generator(builder, steps.initialAtom, steps.repeatingAtom): SequenceGenerator[GnirsDynamicConfig]
        ).value
