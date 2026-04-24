// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import io.circe.Encoder
import lucuma.core.enums.Site
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given

final case class TelescopeModeStatus(interval: TimestampInterval, site: Site, mode: TelescopeMode)
    derives Encoder.AsObject
