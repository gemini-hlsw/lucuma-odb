// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Md5Hash

/**
  * Data classes for the telluric targets service
  * Heavily inspired by Obscalc
  */
object TelluricTargets:
  /**
   * Metadata for each target we need to resolve
   */
  case class Meta(
    observationId:        Observation.Id,
    programId:            Program.Id,
    scienceObservationId: Observation.Id,
    state:                CalculationState,
    lastInvalidation:     Timestamp,
    lastUpdate:           Timestamp,
    retryAt:              Option[Timestamp],
    failureCount:         Int,
    resolvedTargetId:     Option[Target.Id],
    errorMessage:         Option[String],
    scienceDuration:      TimeSpan,
    calibrationOrder:     TelluricCalibrationOrder
  )

  /**
   * Identifies an observation that needs resolving
   */
  case class Pending(
    observationId:        Observation.Id,
    programId:            Program.Id,
    scienceObservationId: Observation.Id,
    lastInvalidation:     Timestamp,
    failureCount:         Int,
    scienceDuration:      TimeSpan,
    paramsHash:           Option[Md5Hash],
    calibrationOrder:     TelluricCalibrationOrder
  )
