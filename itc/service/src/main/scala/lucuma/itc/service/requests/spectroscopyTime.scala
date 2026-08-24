// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import grackle.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.GmosIfuAnalysis
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.GmosSouthFpuParam
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import lucuma.itc.service.hashes.given

case class SpectroscopyTimeParameters(
  exposureTimeMode: ExposureTimeMode,
  specMode:         ObservingMode.SpectroscopyMode,
  constraints:      ItcObservingConditions
) derives Hash

case class TargetSpectroscopyTimeRequest(
  target:     TargetData,
  parameters: SpectroscopyTimeParameters
) extends ServiceRequest derives Hash:
  export parameters.*

case class AsterismSpectroscopyTimeRequest(
  asterism:   NonEmptyChain[TargetData],
  parameters: SpectroscopyTimeParameters
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetSpectroscopyTimeRequest] =
    asterism.map:
      TargetSpectroscopyTimeRequest(_, parameters)

object AsterismSpectroscopyTimeRequest:
  def fromInput(input: SpectroscopyTimeInput): Result[AsterismSpectroscopyTimeRequest] = {
    val SpectroscopyTimeInput(asterism, constraints, mode) =
      input

    val exposureTimeMode = mode.exposureTimeMode

    // The legacy recipe rejects an IFU analysis without an IFU focal plane unit, so catch the
    // contradiction here and say which half to change rather than passing it through.
    def ifuAnalysisFor(
      isIfu:    Boolean,
      analysis: Option[GmosIfuAnalysis]
    ): Result[Option[GmosIfuAnalysis]] =
      if !isIfu && analysis.isDefined then
        Result.failure:
          "'ifuAnalysis' applies only to an IFU focal plane unit; remove it or select an IFU."
      else Result.success(analysis)

    val modeResult: Result[ObservingMode.SpectroscopyMode] =
      mode match
        case GmosNSpectroscopyInput(
              centralWavelength = centralWavelength,
              grating = grating,
              fpu = fpu,
              filter = filter,
              ccdMode = ccdMode,
              roi = roi,
              port = port,
              ifuAnalysis = ifuAnalysis
            ) =>
          val fpuParam = GmosNorthFpuParam(fpu)
          ifuAnalysisFor(fpuParam.isIfu, ifuAnalysis).map: analysis =>
            ObservingMode.SpectroscopyMode
              .GmosNorth(centralWavelength, grating, fpuParam, filter, ccdMode, roi, port, analysis)
        case GmosSSpectroscopyInput(
              centralWavelength = centralWavelength,
              grating = grating,
              fpu = fpu,
              filter = filter,
              ccdMode = ccdMode,
              roi = roi,
              port = port,
              ifuAnalysis = ifuAnalysis
            ) =>
          val fpuParam = GmosSouthFpuParam(fpu)
          ifuAnalysisFor(fpuParam.isIfu, ifuAnalysis).map: analysis =>
            ObservingMode.SpectroscopyMode
              .GmosSouth(centralWavelength, grating, fpuParam, filter, ccdMode, roi, port, analysis)
        case Flamingos2SpectroscopyInput(
              _,
              disperser,
              filter,
              readMode,
              fpu,
              port
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode.Flamingos2(disperser, filter, readMode, fpu, port)
        case Igrins2SpectroscopyInput(_, port) =>
          Result.success:
            ObservingMode.SpectroscopyMode.Igrins2(port)
        case GhostSpectroscopyInput(
              numSkyMicrolens,
              stepCount,
              resolutionMode,
              redDetector,
              blueDetector
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode.Ghost(
              numSkyMicrolens,
              stepCount,
              resolutionMode,
              redDetector,
              blueDetector
            )
        case GnirsSpectroscopyInput(
              _,
              centralWavelength,
              filter,
              fpu,
              prism,
              grating,
              camera,
              readMode,
              wellDepth,
              coadds,
              port
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode.GnirsSpectroscopy(
              centralWavelength,
              filter,
              fpu,
              prism,
              grating,
              camera,
              readMode,
              wellDepth,
              coadds,
              port
            )
        case _                                 =>
          Result.failure("Invalid spectroscopy mode")

    (asterism.targetInputsToData, modeResult, constraints.create).parMapN:
      (asterism, mode, conditions) =>
        AsterismSpectroscopyTimeRequest(
          asterism,
          SpectroscopyTimeParameters(exposureTimeMode, mode, conditions)
        )
  }
