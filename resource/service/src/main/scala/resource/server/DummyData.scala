// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import cats.data.NonEmptyList
import lucuma.core.enums.Site
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import resource.model.*

import java.time.LocalDate

object DummyData {

  val mockTelescopeNightTimelines: NonEmptyList[TelescopeNightTimeline] = NonEmptyList.of(
    TelescopeNightTimeline(
      Site.GN,
      LocalDate.parse("2026-08-01"),
      TimestampInterval.between(
        Timestamp.FromString.unsafeGet("2026-08-02T04:00:00Z"),
        Timestamp.FromString.unsafeGet("2026-08-02T15:30:00Z")
      ),
      List(
        TelescopeAvailabilityStatus(
          TimestampInterval.between(
            Timestamp.FromString.unsafeGet("2026-08-02T04:00:00Z"),
            Timestamp.FromString.unsafeGet("2026-08-02T15:30:00Z")
          ),
          TelescopeAvailability.Open,
          None,
          None,
          Site.GN
        )
      ),
      List(
        TelescopeTooStatus(
          TimestampInterval.between(
            Timestamp.FromString.unsafeGet("2026-08-02T04:00:00Z"),
            Timestamp.FromString.unsafeGet("2026-08-02T10:00:00Z")
          ),
          TooSupport.Rapid,
          Site.GN
        ),
        TelescopeTooStatus(
          TimestampInterval.between(
            Timestamp.FromString.unsafeGet("2026-08-02T10:00:00Z"),
            Timestamp.FromString.unsafeGet("2026-08-02T15:30:00Z")
          ),
          TooSupport.Interrupt,
          Site.GN
        )
      ),
      List(
        TelescopeModeStatus(
          TimestampInterval.between(
            Timestamp.FromString.unsafeGet("2026-08-02T04:00:00Z"),
            Timestamp.FromString.unsafeGet("2026-08-02T15:30:00Z")
          ),
          Site.GN,
          TelescopeMode(
            TelescopeModeType.Queue,
            None
          )
        )
      )
    )
  )
}
