// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.*
import lucuma.core.enums.ExecutionEnvironment as _
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.GmosSouthFpuParam
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import lucuma.itc.service.hashes.given

case class GraphParameters(
  atWavelength: Wavelength,
  specMode:     ObservingMode.SpectroscopyMode,
  constraints:  ItcObservingConditions,
  expTime:      TimeSpan,
  exp:          PosInt
) derives Hash

case class TargetGraphRequest(
  target:     TargetData,
  parameters: GraphParameters
) extends ServiceRequest derives Hash:
  export parameters.*

case class AsterismGraphRequest(
  asterism:           NonEmptyChain[TargetData],
  parameters:         GraphParameters,
  significantFigures: Option[SignificantFigures]
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetGraphRequest] =
    asterism.map:
      TargetGraphRequest(_, parameters)

object AsterismGraphRequest:
  def fromInput(input: SpectroscopyGraphsInput): Result[AsterismGraphRequest] = {
    val SpectroscopyGraphsInput(
      atWavelength,
      exposureTime,
      exposureCount,
      asterism,
      constraints,
      mode,
      figures
    ) = input

    val modeResult: Result[ObservingMode.SpectroscopyMode] = mode match {
      case GmosNSpectroscopyInput(
            centralWavelength,
            grating,
            GmosFpuMask.Builtin(fpu),
            filter,
            ccdMode,
            roi
          ) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosNorth(centralWavelength, grating, GmosNorthFpuParam(fpu), filter, ccdMode, roi)
        )
      case GmosSSpectroscopyInput(
            centralWavelength,
            grating,
            GmosFpuMask.Builtin(fpu),
            filter,
            ccdMode,
            roi
          ) =>
        Result(
          ObservingMode.SpectroscopyMode
            .GmosSouth(centralWavelength, grating, GmosSouthFpuParam(fpu), filter, ccdMode, roi)
        )
      case _ =>
        Result.failure("Invalid spectroscopy mode")
    }

    (asterism.targetInputsToData, modeResult, constraints.create).parMapN:
      (asterism, mode, conditions) =>
        AsterismGraphRequest(
          asterism,
          GraphParameters(
            atWavelength,
            mode,
            conditions,
            exposureTime,
            exposureCount
          ),
          figures
        )
  }
