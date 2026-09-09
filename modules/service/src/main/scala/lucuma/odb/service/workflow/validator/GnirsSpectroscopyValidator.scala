// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcScience
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.gnirs.spectroscopy.Config
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

/**
 * GNIRS spectroscopy checks ported from the OCS p2checker `GnirsRule`.  The
 * checks that need only the observing mode run in the first validation stage
 * (`configuration`); those that need the ITC exposure time run in the second
 * (`exposure`).  Checks on values the ODB derives itself (decker, automatic
 * acquisition filter, read mode when not explicit) are not ported since they
 * cannot fail.
 */
object GnirsSpectroscopyValidator:

  val CrossDispersedThermal: String =
    "Cross-dispersed mode is not available in the L or M bands."

  val ShortBlueLxd: String =
    "The short blue camera must be used with the SXD prism."

  val RedCameraAcquisitionFilter: String =
    "Acquisitions with the red cameras must be done in the PAH, H, K or H2 filters."

  val BlueCameraAcquisitionFilter: String =
    "Acquisitions with the blue cameras must be done in the X, J, H, H2 or K band filters."

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

  def filterMismatch(filter: GnirsFilter, wavelength: Wavelength): String =
    s"Filter ${filter.shortName} does not cover the central wavelength ${formatWavelength(wavelength)}."

  def exposureTooShort(readMode: GnirsReadMode, wavelength: Wavelength): String =
    s"Exposure times for ${readMode.shortName} read mode must be at least ${formatSeconds(readMode.minimumExposureTime)} s (at ${formatWavelength(wavelength)})."

  def exposureUnusuallyLong(readMode: GnirsReadMode, max: TimeSpan, wavelength: Wavelength): String =
    s"Exposure times for ${readMode.shortName} read mode are normally less than ${formatSeconds(max)} s; consider using a lower read noise mode (at ${formatWavelength(wavelength)})."

  private def formatWavelength(wavelength: Wavelength): String =
    f"${Wavelength.decimalMicrometers.reverseGet(wavelength)}%.3f µm"

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

  val configuration: ObservationValidator = info =>
    config(info).foldMap: c =>
      val wavelengths: List[Wavelength] =
        c.wavelengths.toList.map(_.centralWavelength)

      val crossDispersedThermal: ObservationValidationMap =
        when(c.prism =!= GnirsPrism.Mirror && wavelengths.exists(isThermal))(error(CrossDispersedThermal))

      val shortBlueLxd: ObservationValidationMap =
        when(c.camera === GnirsCamera.ShortBlue && c.prism === GnirsPrism.Lxd)(error(ShortBlueLxd))

      // The XD filter has no range of its own, so it is never checked.
      val filterCoverage: ObservationValidationMap =
        c.filter.spectroscopyRange.foldMap: range =>
          wavelengths.filterNot(range.contains).foldMap(w => warning(filterMismatch(c.filter, w)))

      // Automatic selection is always valid; only an explicit choice can be wrong.
      val acquisitionFilter: ObservationValidationMap =
        c.acquisition.explicitFilter.foldMap: f =>
          if isThermal(c.primaryCentralWavelength) then when(!RedCameraAcquisitionFilters(f))(error(RedCameraAcquisitionFilter))
          else when(!BlueCameraAcquisitionFilters(f))(error(BlueCameraAcquisitionFilter))

      crossDispersedThermal |+| shortBlueLxd |+| filterCoverage |+| acquisitionFilter

  def exposure(itcFor: Observation.Id => Option[Itc]): ObservationValidator = info =>
    (config(info), itcFor(info.oid).map(_.science)).tupled.foldMap:
      case (c, ItcScience.GnirsSpectroscopy(science)) =>
        science.toNel.foldMap: (w, z) =>
          exposureChecks(c, w, z.focus.value.exposureTime)
      case _                                          =>
        ObservationValidationMap.empty

  private def exposureChecks(c: Config, wavelength: Wavelength, exposure: TimeSpan): ObservationValidationMap =
    val readMode: GnirsReadMode =
      c.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(exposure))
    if exposure < readMode.minimumExposureTime then error(exposureTooShort(readMode, wavelength))
    else
      UsualMaximumExposureTime
        .get(readMode)
        .filter(exposure > _)
        .foldMap(max => warning(exposureUnusuallyLong(readMode, max, wavelength)))
