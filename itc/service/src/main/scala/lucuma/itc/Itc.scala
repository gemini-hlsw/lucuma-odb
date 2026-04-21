// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.TargetGraphsCalcResult
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetTimeAndGraphs

trait Itc[F[_]]:

  /**
   * Retrieve the integration time for the given mode and exposureTimeMode.
   */
  def calculate(
    target:           TargetData,
    observingMode:    ObservingMode,
    constraints:      ItcObservingConditions,
    exposureTimeMode: ExposureTimeMode
  ): F[TargetIntegrationTime]

  /**
   * Retrieve the graph data for the given mode and exposureTime and exposures
   */
  def calculateGraphs(
    target:        TargetData,
    atWavelength:  Wavelength,
    observingMode: ObservingMode,
    constraints:   ItcObservingConditions,
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ): F[TargetGraphsCalcResult]

  def calculateTimeAndGraphs(
    target:           TargetData,
    observingMode:    ObservingMode,
    constraints:      ItcObservingConditions,
    exposureTimeMode: ExposureTimeMode
  ): F[TargetTimeAndGraphs]

object Itc:
  def apply[F[_]](using ev: Itc[F]): ev.type = ev
