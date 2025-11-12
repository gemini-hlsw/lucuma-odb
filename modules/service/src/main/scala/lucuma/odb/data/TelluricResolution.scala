// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp

object TelluricResolution:
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
    errorMessage:         Option[String]
  )

  case class Pending(
    observationId:        Observation.Id,
    programId:            Program.Id,
    scienceObservationId: Observation.Id,
    lastInvalidation:     Timestamp,
    failureCount:         Int
  )
