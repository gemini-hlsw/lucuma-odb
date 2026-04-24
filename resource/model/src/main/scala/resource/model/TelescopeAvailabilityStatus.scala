// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import io.circe.Encoder
import lucuma.core.enums.Site
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given

final case class TelescopeAvailabilityStatus(
  interval:            TimestampInterval,
  availability:        TelescopeAvailability,
  reason:              Option[String],
  plannedAvailability: Option[TelescopeAvailability],
  site:                Site
) derives Encoder.AsObject
