// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

/**
 * Altair (Gemini North adaptive optics) checks, after the OCS p2checker rules as
 * revised by the GNIRS team (September 2026). Only runs when the observation
 * carries an Altair configuration. What the API and the database already refuse
 * (Altair on an unsupported instrument, the field lens out or the ND filter in
 * with the laser) is not repeated here, and whether a guide star exists is only
 * known once AGS has run, so the sequence calculation reports that instead.
 */
object AltairValidator extends ObservationValidator:

  val LgsP1LongSlitMessage: String =
    "Altair LGS+P1 on a long slit is subject to flexure; we recommend that a continuum source be visible in the slit."

  val LgsConditionsMessage: String =
    "Altair LGS requires clear skies: cloud extinction below 0.1 mag."

  val WavelengthTooLongMessage: String =
    "Altair does not help at wavelengths longer than 4.3 µm."

  val WavelengthTooShortMessage: String =
    "Altair typically provides Strehl ratios of 5% or less for wavelengths shorter than 1.3 µm."

  private val WavelengthCeiling: Wavelength =
    Wavelength.unsafeFromIntPicometers(4_300_000)

  private val WavelengthFloor: Wavelength =
    Wavelength.unsafeFromIntPicometers(1_300_000)

  private def error(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.configuration(msg))

  private def warning(msg: String): ObservationValidationMap =
    ObservationValidationMap.singleton(ObservationValidation.Warning.configuration(msg))

  // The laser needs clear skies; the presets step from 0 to 0.1 mag, so only the clearest passes.
  private val LaserCloudExtinctionLimit: BigDecimal =
    BigDecimal("0.1")

  private def conditionsTooPoorForLaser(info: ObservationValidationInfo): Boolean =
    info.constraintSet.cloudExtinction.toCloudExtinction.toVegaMagnitude >= LaserCloudExtinctionLimit

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
      val lgsP1LongSlit: ObservationValidationMap =
        Option.when(altair.mode === AltairMode.LgsP1 && info.observingMode.contains(ObservingModeType.GnirsLongSlit))(warning(LgsP1LongSlitMessage)).orEmpty

      val lgsConditions: ObservationValidationMap =
        Option.when(altair.mode.usesLaser && conditionsTooPoorForLaser(info))(error(LgsConditionsMessage)).orEmpty

      val wavelengths: List[Wavelength] =
        scienceWavelengths(info)

      val wavelengthTooLong: ObservationValidationMap =
        Option.when(wavelengths.exists(_ > WavelengthCeiling))(error(WavelengthTooLongMessage)).orEmpty

      val wavelengthTooShort: ObservationValidationMap =
        Option.when(wavelengths.exists(_ < WavelengthFloor))(warning(WavelengthTooShortMessage)).orEmpty

      lgsP1LongSlit |+| lgsConditions |+| wavelengthTooLong |+| wavelengthTooShort
