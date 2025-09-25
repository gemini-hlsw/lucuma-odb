// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp

case class VisitRecord(
  visitId:        Visit.Id,
  observationId:  Observation.Id,
  instrument:     Instrument,
  created:        Timestamp,
  site:           Site,
  chargeable:     Boolean,
  idempotencyKey: Option[IdempotencyKey]
)