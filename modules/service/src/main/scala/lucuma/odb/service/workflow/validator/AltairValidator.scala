// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMass
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.service.AltairRules
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

/**
 * Altair (Gemini North adaptive optics) checks ported from the OCS p2checker
 * `AltairRule` / `GnirsRule` and the Gemini Altair web pages. Only runs when
 * the observation carries an Altair configuration.
 */
object AltairValidator extends ObservationValidator:

  val NdFilterLgsMessage: String =
    "The Altair ND filter is not commissioned for LGS; set it to OUT."

  val LgsP1LongSlitMessage: String =
    "Altair LGS+P1 is not commissioned for long slit spectroscopy; use NGS or LGS, or an IFU or imaging mode."

  val MissingGuideStarMessage: String =
    "Altair NGS and LGS need a selected guide star; LGS+P1 does not."

  val LgsConditionsMessage: String =
    "Altair LGS requires cloud cover 50% or better and image quality 70% or better."

  val WavelengthTooLongMessage: String =
    "Altair cannot be used at wavelengths of 4.3 µm and longer."

  val WavelengthTooShortMessage: String =
    "Altair typically provides Strehl ratios of 5% or less for wavelengths shorter than 1.3 µm."

  private val WavelengthCeiling: Wavelength =
    Wavelength.unsafeFromIntPicometers(4_300_000)

  private val WavelengthFloor: Wavelength =
    Wavelength.unsafeFromIntPicometers(1_300_000)

  // Imaging has no spectroscopy central wavelength; Altair's own optimal
  // wavelength (H band) stands in for the wavelength checks in that case.
  private val ImagingFallbackWavelength: Wavelength =
    Wavelength.unsafeFromIntPicometers(1_650_000)

  private def error(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.configuration(msg))

  private def warning(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.Warning.configuration(msg))

  private def when(cond: Boolean)(v: => ObservationValidationMap): ObservationValidationMap =
    if cond then v else ObservationValidationMap.empty

  // Air mass 1 (zenith) is the reference the condition presets are quoted at.
  private val ZenithAirMass: AirMass =
    AirMass.unsafeFrom(BigDecimal(1))

  private def conditionsTooPoorForLaser(info: ObservationValidationInfo): Boolean =
    val wavelength = info.spectroscopyWavelength.getOrElse(ImagingFallbackWavelength)
    val cloudPercentile = info.constraintSet.cloudExtinction.toCloudExtinction.percentile
    val seeingPercentile = info.constraintSet.imageQuality.toImageQuality.percentile(wavelength, ZenithAirMass)
    cloudPercentile.toPercent > 50 || seeingPercentile.toPercent > 70

  // Every central science wavelength Altair actually observes at: every GNIRS
  // spectroscopy central wavelength (split spectroscopy repeats the wavelength
  // checks per occurrence), or every GNIRS imaging filter's central wavelength.
  // Empty for any other mode, or before the generator params are known.
  private def scienceWavelengths(info: ObservationValidationInfo): List[Wavelength] =
    info
      .generatorParams
      .flatMap(_.toOption)
      .map(_.observingMode)
      .flatMap: mode =>
        mode
          .asGnirsSpectroscopy.map(_.wavelengths.toList.map(_.centralWavelength))
          .orElse(mode.asGnirsImaging.map(_.filters.toList.map(_.filter.centralWavelength)))
      .orEmpty

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.altair.foldMap: altair =>
      val notGnirs: ObservationValidationMap =
        when(!info.instrument.contains(Instrument.Gnirs))(error(AltairRules.NotGnirsMessage))

      val ndFilterLgs: ObservationValidationMap =
        when(altair.mode.usesLaser && altair.ndFilter === AltairNdFilter.In)(error(NdFilterLgsMessage))

      val lgsP1LongSlit: ObservationValidationMap =
        when(altair.mode === AltairMode.LgsP1 && info.observingMode.contains(ObservingModeType.GnirsLongSlit))(error(LgsP1LongSlitMessage))

      val lgsConditions: ObservationValidationMap =
        when(altair.mode.usesLaser && conditionsTooPoorForLaser(info))(error(LgsConditionsMessage))

      // The guide star sets the Strehl the ITC models, and the NGS field lens position, so both
      // the exposure times and the sequence depend on it. LGS+P1 guides with PWFS1 and needs none.
      val missingGuideStar: ObservationValidationMap =
        when(altair.mode =!= AltairMode.LgsP1 && !info.hasGuideTargetName)(error(MissingGuideStarMessage))

      val wavelengths: List[Wavelength] =
        scienceWavelengths(info)

      val wavelengthTooLong: ObservationValidationMap =
        when(wavelengths.exists(_ >= WavelengthCeiling))(error(WavelengthTooLongMessage))

      val wavelengthTooShort: ObservationValidationMap =
        when(wavelengths.exists(_ < WavelengthFloor))(warning(WavelengthTooShortMessage))

      notGnirs |+| ndFilterLgs |+| lgsP1LongSlit |+| lgsConditions |+| missingGuideStar |+| wavelengthTooLong |+| wavelengthTooShort
