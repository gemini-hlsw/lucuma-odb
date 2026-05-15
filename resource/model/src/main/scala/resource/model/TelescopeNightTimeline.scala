// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import io.circe.Encoder
import lucuma.core.enums.Site
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given

import java.time.LocalDate

final case class TelescopeNightTimeline(
  site:            Site,
  observingNight:  LocalDate,
  displayInterval: TimestampInterval,
  availability:    List[TelescopeAvailabilityStatus],
  tooStatus:       List[TelescopeTooStatus],
  modes:           List[TelescopeModeStatus]
) derives Encoder.AsObject
