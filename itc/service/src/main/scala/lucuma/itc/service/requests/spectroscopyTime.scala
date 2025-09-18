// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import grackle.*
import lucuma.core.enums.ExecutionEnvironment as _
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
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
    val SpectroscopyTimeInput(exposureTimeMode, asterism, constraints, mode) =
      input

    val modeResult: Result[ObservingMode.SpectroscopyMode] =
      mode match
        case GmosNSpectroscopyInput(
              centralWavelength,
              grating,
              GmosFpuMask.Builtin(fpu),
              filter,
              ccdMode,
              roi
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosNorth(centralWavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        case GmosSSpectroscopyInput(
              centralWavelength,
              grating,
              GmosFpuMask.Builtin(fpu),
              filter,
              ccdMode,
              roi
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode
              .GmosSouth(centralWavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        case Flamingos2SpectroscopyInput(
              disperser,
              filter,
              fpu
            ) =>
          Result.success:
            ObservingMode.SpectroscopyMode.Flamingos2(disperser, filter, fpu)
        case _ =>
          Result.failure("Invalid spectroscopy mode")

    (asterism.targetInputsToData, modeResult, constraints.create).parMapN:
      (asterism, mode, conditions) =>
        AsterismSpectroscopyTimeRequest(
          asterism,
          SpectroscopyTimeParameters(exposureTimeMode, mode, conditions)
        )
  }
