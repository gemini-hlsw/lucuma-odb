// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcScience
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.gnirs.spectroscopy.Config
import lucuma.odb.sequence.gnirs.wavelengthOccurrences
import lucuma.odb.sequence.gnirs.withOccurrence
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

/**
 * GNIRS spectroscopy checks ported from the OCS p2checker `GnirsRule`.  The
 * checks that need only the observing mode run in the first validation stage
 * (`configuration`); those that need the ITC exposure time run in the second
 * (`exposure`).  Values the ODB derives itself can only be wrong when the user
 * overrides them, so the decker and read mode checks fire on explicit choices
 * and the acquisition filter check on an explicit filter.
 */
object GnirsSpectroscopyValidator:

  val CrossDispersedThermal: String =
    "Cross-dispersed mode is not available in the L or M bands."

  val ShortBlueLxd: String =
    "The short blue camera cannot be used with the LXD prism."

  val RedCameraCrossDispersed: String =
    "The red cameras cannot be used in cross-dispersed mode."

  val RedCameraAcquisitionFilter: String =
    "Acquisitions with the red cameras must be done in the PAH, H, K or H2 filters."

  val BlueCameraAcquisitionFilter: String =
    "Acquisitions with the blue cameras must be done in the X, J, H, H2 or K band filters."

  val DeckerAcquisitionMirror: String =
    "Decker does not match Acquisition Mirror."

  def deckerMismatch(decker: GnirsDecker, expected: GnirsDecker): String =
    s"Decker ${decker.longName} does not match the FPU, camera and prism (expected ${expected.longName})."

  val RedCameras: Set[GnirsCamera] =
    Set(GnirsCamera.ShortRed, GnirsCamera.LongRed)

  val RedCameraAcquisitionFilters: Set[GnirsFilter] =
    Set(GnirsFilter.PAH, GnirsFilter.Order4, GnirsFilter.Order3, GnirsFilter.H2)

  val BlueCameraAcquisitionFilters: Set[GnirsFilter] =
    Set(GnirsFilter.Order6, GnirsFilter.Order5, GnirsFilter.Order4, GnirsFilter.H2, GnirsFilter.Order3)

  // Longer exposures are legal, but a lower read noise mode is normally used instead.
  val UsualMaximumExposureTime: Map[GnirsReadMode, TimeSpan] =
    Map(
      GnirsReadMode.VeryBright -> 1.secTimeSpan,
      GnirsReadMode.Bright     -> 20.secTimeSpan,
      GnirsReadMode.Faint      -> 60.secTimeSpan
    )

  def filterMismatch(filter: GnirsFilter, wavelength: Wavelength, occurrence: Option[Int] = None): String =
    s"Filter ${filter.shortName} does not cover the central wavelength ${formatWavelength(wavelength, occurrence)}."

  def exposureTooShort(readMode: GnirsReadMode, wavelength: Wavelength, occurrence: Option[Int] = None): String =
    s"Exposure times for ${readMode.shortName} read mode must be at least ${formatSeconds(readMode.minimumExposureTime)} s (at ${formatWavelength(wavelength, occurrence)})."

  def exposureUnusuallyLong(readMode: GnirsReadMode, max: TimeSpan, wavelength: Wavelength, occurrence: Option[Int] = None): String =
    s"Exposure times for ${readMode.shortName} read mode are normally less than ${formatSeconds(max)} s; consider using a lower read noise mode (at ${formatWavelength(wavelength, occurrence)})."

  /**
   * Names a central wavelength in a message.  A wavelength the observation repeats is
   * given its 1-based occurrence ordinal, because each occurrence is an independent
   * configuration and the observer has to know which one the message is about; a
   * wavelength that occurs once is named exactly as it always was.  The ordinal comes
   * from `gnirs.wavelengthOccurrences`, shared with the sequence atom titles and the
   * low signal-to-noise warning.
   */
  private def formatWavelength(wavelength: Wavelength, occurrence: Option[Int]): String =
    withOccurrence(f"${Wavelength.decimalMicrometers.reverseGet(wavelength)}%.3f µm", occurrence)

  private def formatSeconds(time: TimeSpan): String =
    time.toSeconds.bigDecimal.stripTrailingZeros.toPlainString

  private def config(info: ObservationValidationInfo): Option[Config] =
    info.generatorParams.flatMap(_.toOption).flatMap(_.observingMode.asGnirsSpectroscopy)

  private def error(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.configuration(msg))

  private def warning(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.Warning.configuration(msg))

  private def when(cond: Boolean)(v: => ObservationValidationMap): ObservationValidationMap =
    if cond then v else ObservationValidationMap.empty

  private def isThermal(wavelength: Wavelength): Boolean =
    wavelength >= GnirsFilter.ThermalAcquisitionCutoff

  // The same derivation the DB view uses for the default decker.
  private def defaultDecker(c: Config): GnirsDecker =
    c.fpu match
      case GnirsFpu.Spectroscopy.Slit(value = _) => GnirsDecker.forCameraAndPrism(c.camera, c.prism)
      case GnirsFpu.Spectroscopy.Ifu(value = i)  => GnirsDecker.forIfu(i)

  val configuration: ObservationValidator = info =>
    config(info).foldMap: c =>
      val wavelengths: List[Wavelength] =
        c.wavelengths.toList.map(_.centralWavelength)

      val crossDispersedThermal: ObservationValidationMap =
        when(c.prism =!= GnirsPrism.Mirror && wavelengths.exists(isThermal))(error(CrossDispersedThermal))

      val shortBlueLxd: ObservationValidationMap =
        when(c.camera === GnirsCamera.ShortBlue && c.prism === GnirsPrism.Lxd)(error(ShortBlueLxd))

      // Not offered by the configuration options, but the camera can be set directly.
      val redCameraCrossDispersed: ObservationValidationMap =
        when(RedCameras(c.camera) && c.prism =!= GnirsPrism.Mirror)(error(RedCameraCrossDispersed))

      // The XD filter has no range of its own, so it is never checked.
      val filterCoverage: ObservationValidationMap =
        c.filter.spectroscopyRange.foldMap: range =>
          wavelengths
            .zip(wavelengthOccurrences(wavelengths))
            .collect { case (w, occ) if !range.contains(w) => warning(filterMismatch(c.filter, w, occ)) }
            .combineAll

      // Science steps run with the acquisition mirror out, and the acquisition
      // steps always use the acquisition decker, so an explicit acquisition
      // decker can only be a mistake.
      val decker: ObservationValidationMap =
        if c.decker === GnirsDecker.Acquisition then warning(DeckerAcquisitionMirror)
        else when(c.decker =!= defaultDecker(c))(warning(deckerMismatch(c.decker, defaultDecker(c))))

      // Automatic selection is always valid; only an explicit choice can be wrong.
      val acquisitionFilter: ObservationValidationMap =
        c.acquisition.explicitFilter.foldMap: f =>
          if isThermal(c.primaryCentralWavelength) then when(!RedCameraAcquisitionFilters(f))(error(RedCameraAcquisitionFilter))
          else when(!BlueCameraAcquisitionFilters(f))(error(BlueCameraAcquisitionFilter))

      crossDispersedThermal |+| shortBlueLxd |+| redCameraCrossDispersed |+| filterCoverage |+| decker |+| acquisitionFilter

  def exposure(itcFor: Observation.Id => Option[Itc]): ObservationValidator = info =>
    (config(info), itcFor(info.oid).map(_.science)).tupled.foldMap:
      case (c, ItcScience.GnirsSpectroscopy(science)) =>
        // One ITC result per central wavelength, in list order, so the occurrence
        // ordinals line up with the results positionally.
        val occurrences = wavelengthOccurrences(science.toList.map(_._1))
        science.zipWithIndex.foldMap: (entry, i) =>
          val (w, z) = entry
          exposureChecks(c, w, z.focus.value.exposureTime, occurrences(i))
      case _                                          =>
        ObservationValidationMap.empty

  private def exposureChecks(
    c:          Config,
    wavelength: Wavelength,
    exposure:   TimeSpan,
    occurrence: Option[Int]
  ): ObservationValidationMap =
    val readMode: GnirsReadMode =
      c.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(exposure))
    if exposure < readMode.minimumExposureTime then error(exposureTooShort(readMode, wavelength, occurrence))
    else
      UsualMaximumExposureTime
        .get(readMode)
        .filter(exposure > _)
        .foldMap(max => warning(exposureUnusuallyLong(readMode, max, wavelength, occurrence)))
