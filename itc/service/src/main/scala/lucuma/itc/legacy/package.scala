// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import scala.concurrent.duration.FiniteDuration

enum ItcWavefrontSensor(val ocs2Tag: String):
  case PWFS  extends ItcWavefrontSensor("PWFS")
  case OIWFS extends ItcWavefrontSensor("OIWFS")

case class ItcTelescopeDetails(wfs: ItcWavefrontSensor)

case class ItcSourceDefinition(
  target:     TargetData,
  bandOrLine: Either[Band, Wavelength]
):
  export target.*

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
          coadds = None,
          exposureDuration = exposureDuration,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0,
          wavelengthAt = atWavelength
        ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)

def spectroscopyIntegrationTimeParams(
  target:        TargetData,
  atWavelength:  Wavelength,
  observingMode: ObservingMode.SpectroscopyMode,
  conditions:    ItcObservingConditions,
  sigma:         SignalToNoise
): (ItcParameters, Either[Band, Wavelength]) = // Bubble up the selected band or line
  val (sourceDefinition, bandOrLine): (ItcSourceDefinition, Either[Band, Wavelength]) =
    buildSourceDefinition(target, atWavelength)
  val parameters: ItcParameters                                                       =
    ItcParameters(
      source = sourceDefinition,
      observation = ItcObservationDetails(
        calculationMethod =
          ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
            sigma = sigma.toBigDecimal.toDouble,
            coadds = None,
            wavelengthAt = atWavelength,
            sourceFraction = 1.0,
            ditherOffset = Angle.Angle0
          ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)

def spectroscopySNParams(
  target:           TargetData,
  atWavelength:     Wavelength,
  observingMode:    ObservingMode.SpectroscopyMode,
  conditions:       ItcObservingConditions,
  exposureDuration: FiniteDuration,
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
          exposureDuration = exposureDuration,
          coadds = None,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0,
          wavelengthAt = atWavelength
        ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)

def imagingIntegrationTimeParams(
  target:        TargetData,
  atWavelength:  Wavelength,
  observingMode: ObservingMode,
  conditions:    ItcObservingConditions,
  sigma:         SignalToNoise
): (ItcParameters, Either[Band, Wavelength]) = // Bubble up the selected band or line
  val (sourceDefinition, bandOrLine): (ItcSourceDefinition, Either[Band, Wavelength]) =
    buildSourceDefinition(target, atWavelength)
  val parameters: ItcParameters                                                       =
    ItcParameters(
      source = sourceDefinition,
      observation = ItcObservationDetails(
        calculationMethod =
          ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.ImagingIntegrationTime(
            sigma = sigma.toBigDecimal.toDouble,
            coadds = None,
            sourceFraction = 1.0,
            ditherOffset = Angle.Angle0
          ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)

def imagingS2NParams(
  target:           TargetData,
  atWavelength:     Wavelength,
  observingMode:    ObservingMode,
  conditions:       ItcObservingConditions,
  exposureDuration: FiniteDuration,
  exposureCount:    Int
): (ItcParameters, Either[Band, Wavelength]) = // Bubble up the selected band or line
  val (sourceDefinition, bandOrLine): (ItcSourceDefinition, Either[Band, Wavelength]) =
    buildSourceDefinition(target, atWavelength)
  val parameters: ItcParameters                                                       =
    ItcParameters(
      source = sourceDefinition,
      observation = ItcObservationDetails(
        calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.ImagingS2N(
          exposureCount = exposureCount,
          exposureDuration = exposureDuration,
          coadds = None,
          sourceFraction = 1.0,
          ditherOffset = Angle.Angle0
        ),
        analysisMethod = observingMode.analysisMethod
      ),
      conditions = conditions,
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails(observingMode)
    )
  (parameters, bandOrLine)
