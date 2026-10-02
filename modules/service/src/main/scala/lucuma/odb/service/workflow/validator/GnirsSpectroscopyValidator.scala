// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.Wavelength
import lucuma.core.model.ObservationValidation
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.gnirs.spectroscopy.Config
import lucuma.odb.sequence.gnirs.wavelengthOccurrences
import lucuma.odb.sequence.gnirs.withOccurrence
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

/**
 * GNIRS spectroscopy checks ported from the OCS p2checker `GnirsRule`.  They
 * need only the observing mode.  The read mode exposure time limits are
 * checked against the sequence steps by the `ExposureTimeValidator`.  Values
 * the ODB derives itself can only be wrong when the user overrides them, so
 * the decker check fires on an explicit choice and the acquisition filter
 * check on an explicit filter.
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

  def filterMismatch(filter: GnirsFilter, wavelength: Wavelength, occurrence: Option[Int] = None): String =
    s"Filter ${filter.shortName} does not cover the central wavelength ${formatWavelength(wavelength, occurrence)}."

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

  private def config(info: ObservationValidationInfo): Option[Config] =
    info.generatorParams.flatMap(_.toOption).flatMap(_.observingMode.asGnirsSpectroscopy)

  private def error(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.configuration(msg))

  private def warning(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.Warning.configuration(msg))

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
        Option.when(c.prism =!= GnirsPrism.Mirror && wavelengths.exists(isThermal))(error(CrossDispersedThermal)).orEmpty

      val shortBlueLxd: ObservationValidationMap =
        Option.when(c.camera === GnirsCamera.ShortBlue && c.prism === GnirsPrism.Lxd)(error(ShortBlueLxd)).orEmpty

      // Not offered by the configuration options, but the camera can be set directly.
      val redCameraCrossDispersed: ObservationValidationMap =
        Option.when(RedCameras(c.camera) && c.prism =!= GnirsPrism.Mirror)(error(RedCameraCrossDispersed)).orEmpty

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
        else Option.when(c.decker =!= defaultDecker(c))(warning(deckerMismatch(c.decker, defaultDecker(c)))).orEmpty

      // Automatic selection is always valid; only an explicit choice can be wrong.
      val acquisitionFilter: ObservationValidationMap =
        c.acquisition.explicitFilter.foldMap: f =>
          if isThermal(c.primaryCentralWavelength) then Option.when(!RedCameraAcquisitionFilters(f))(error(RedCameraAcquisitionFilter)).orEmpty
          else Option.when(!BlueCameraAcquisitionFilters(f))(error(BlueCameraAcquisitionFilter)).orEmpty

      crossDispersedThermal |+| shortBlueLxd |+| redCameraCrossDispersed |+| filterCoverage |+| decker |+| acquisitionFilter

