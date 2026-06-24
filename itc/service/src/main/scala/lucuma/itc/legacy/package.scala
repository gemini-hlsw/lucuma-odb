// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.option.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SourceProfile
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import scala.concurrent.duration.*

enum ItcWavefrontSensor(val ocs2Tag: String):
  case PWFS  extends ItcWavefrontSensor("PWFS")
  case OIWFS extends ItcWavefrontSensor("OIWFS")

case class ItcTelescopeDetails(wfs: ItcWavefrontSensor, instrumentPort: PortDisposition)

case class ItcSourceDefinition(
  target:     TargetData,
  bandOrLine: Either[Band, Wavelength]
):
  export target.*

// Diagnostic toggle: set env/sysprop ITC_SEND_COADDS=false to stop sending coadds to the
// legacy ITC (keeping all other coadds plumbing and cache keys), to isolate whether the
// coadds value fed into the legacy recipe is what destabilizes the service. Defaults to true.
private val SendCoaddsToLegacy: Boolean =
  sys.env
    .get("ITC_SEND_COADDS")
    .orElse(sys.props.get("ITC_SEND_COADDS"))
    .forall(s => s.equalsIgnoreCase("true") || s == "1")

extension (mode: ObservingMode)
  /** The number of coadds for instruments that support it (currently GNIRS), else None. */
  def coadds: Option[Int] =
    if !SendCoaddsToLegacy then none
    else
      mode match
        case ObservingMode.SpectroscopyMode.GnirsLongSlit(coadds = coadds) => coadds.value.some
        case ObservingMode.ImagingMode.Gnirs(coadds = coadds)              => coadds.value.some
        case _                                                             => none

extension (etm: ExposureTimeMode)
  def spectroscopyCalculationMethod(
    coadds: Option[Int] = none
  ): ItcObservationDetails.CalculationMethod =
    etm match
      case ExposureTimeMode.SignalToNoiseMode(sn, at)         =>
        ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
          sigma = sn.toBigDecimal.toDouble,
          coadds = coadds,
          wavelengthAt = at,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        )
      case ExposureTimeMode.TimeAndCountMode(time, count, at) =>
        ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
          exposureCount = count.value,
          coadds = coadds,
          exposureDuration = time.toMilliseconds.toDouble.milliseconds,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0,
          wavelengthAt = at
        )
  def imagingCalculationMethod(
    coadds: Option[Int] = none
  ): ItcObservationDetails.CalculationMethod =
    etm match
      case ExposureTimeMode.SignalToNoiseMode(sn, at)         =>
        ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.ImagingIntegrationTime(
          sigma = sn.toBigDecimal.toDouble,
          coadds = coadds,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        )
      case ExposureTimeMode.TimeAndCountMode(time, count, at) =>
        ItcObservationDetails.CalculationMethod.S2NMethod.ImagingS2N(
          exposureCount = count.value,
          coadds = coadds,
          exposureDuration = time.toMilliseconds.toDouble.milliseconds,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        )

  def desiredString: String =
    etm match
      case ExposureTimeMode.SignalToNoiseMode(sn, at)         => s"Desired S/N $sn at $at"
      case ExposureTimeMode.TimeAndCountMode(time, count, at) => // TODO add coadds
        s"Calculate S/N for exp time $time and count $count at $at"

def getCalculationMethod(
  observingMode:    ObservingMode,
  exposureTimeMode: ExposureTimeMode
): ItcObservationDetails.CalculationMethod =
  observingMode match
    case s: ObservingMode.SpectroscopyMode =>
      exposureTimeMode.spectroscopyCalculationMethod(observingMode.coadds)
    case i: ObservingMode.ImagingMode      =>
      exposureTimeMode.imagingCalculationMethod(observingMode.coadds)

case class ItcParameters(
  source:      ItcSourceDefinition,
  observation: ItcObservationDetails,
  conditions:  ItcObservingConditions,
  telescope:   ItcTelescopeDetails,
  instrument:  ItcInstrumentDetails
)

case class ItcInstrumentDetails(mode: ObservingMode)

private def buildSourceDefinition(
  target:       TargetData,
  atWavelength: Wavelength
): (ItcSourceDefinition, Either[Band, Wavelength]) =
  val bandOrLine: Either[Band, Wavelength] = target.bandOrLine(atWavelength)
  (ItcSourceDefinition(target, bandOrLine), bandOrLine)

/** Convert model types into OCS2 ITC-compatible types for a spectroscopy request. */
def spectroscopyGraphParams(
  target:           TargetData,
  atWavelength:     Wavelength,
  observingMode:    ObservingMode,
  exposureDuration: FiniteDuration,
  conditions:       ItcObservingConditions,
  exposureCount:    Int
): (ItcParameters, Either[Band, Wavelength]) = // Bubble up the selected band or line
  val (sourceDefinition, bandOrLine): (ItcSourceDefinition, Either[Band, Wavelength]) =
    buildSourceDefinition(target, atWavelength)
  val parameters: ItcParameters                                                       =
    ItcParameters(
      source = sourceDefinition,
      observation = ItcObservationDetails(
        calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
          exposureCount = exposureCount,
          coadds = observingMode.coadds,
          exposureDuration = exposureDuration,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0,
          wavelengthAt = atWavelength
        ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS,
        instrumentPort = observingMode.portDisposition
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)

def toItcParameters(
  target:           TargetData,
  observingMode:    ObservingMode,
  conditions:       ItcObservingConditions,
  exposureTimeMode: ExposureTimeMode
): (ItcParameters, Either[Band, Wavelength]) = // Bubble up the selected band or line
  val (sourceDefinition, bandOrLine): (ItcSourceDefinition, Either[Band, Wavelength]) =
    buildSourceDefinition(target, exposureTimeMode.at)
  val parameters: ItcParameters                                                       =
    ItcParameters(
      source = sourceDefinition,
      observation = ItcObservationDetails(
        calculationMethod = getCalculationMethod(observingMode, exposureTimeMode),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS,
        instrumentPort = observingMode.portDisposition
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)
