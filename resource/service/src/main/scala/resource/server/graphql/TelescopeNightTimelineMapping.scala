// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.skunk.SkunkMapping

trait TelescopeNightTimelineMapping[F[_]] extends TimestampIntervalMapping[F]:
  this: SkunkMapping[F] =>

  import TelescopeAvailabilityStatusTable as Avail
  import TelescopeModeStatusTable as Mode
  import TelescopeNightTimelineTable as Timeline
  import TelescopeTooStatusTable as Too

  lazy val TelescopeNightTimelineMappings: List[TypeMapping] =
    List(
      ObjectMapping(TelescopeNightTimelineType)(
        SqlField("site", Timeline.Site, key = true),
        SqlField("observingNight", Timeline.ObservingNight, key = true),
        SqlObject("displayInterval"),
        SqlObject(
          "availability",
          Join(List(Timeline.Site -> Avail.Site, Timeline.ObservingNight -> Avail.ObservingNight))
        ),
        SqlObject(
          "tooStatus",
          Join(List(Timeline.Site -> Too.Site, Timeline.ObservingNight -> Too.ObservingNight))
        ),
        SqlObject(
          "modes",
          Join(List(Timeline.Site -> Mode.Site, Timeline.ObservingNight -> Mode.ObservingNight))
        )
      ),

      ObjectMapping(TelescopeAvailabilityStatusType)(
        SqlField("_key_site", Avail.Site, key = true, hidden = true),
        SqlField("_key_night", Avail.ObservingNight, key = true, hidden = true),
        SqlField("_key_start", Avail.IntervalStart, key = true, hidden = true),
        SqlObject("interval"),
        SqlField("availability", Avail.Availability),
        SqlField("reason", Avail.Reason),
        SqlField("plannedAvailability", Avail.PlannedAvailability),
        SqlField("site", Avail.Site)
      ),

      ObjectMapping(TelescopeTooStatusType)(
        SqlField("_key_site", Too.Site, key = true, hidden = true),
        SqlField("_key_night", Too.ObservingNight, key = true, hidden = true),
        SqlField("_key_start", Too.IntervalStart, key = true, hidden = true),
        SqlObject("interval"),
        SqlField("tooSupport", Too.TooSupport),
        SqlField("site", Too.Site)
      ),

      ObjectMapping(TelescopeModeStatusType)(
        SqlField("_key_site", Mode.Site, key = true, hidden = true),
        SqlField("_key_night", Mode.ObservingNight, key = true, hidden = true),
        SqlField("_key_start", Mode.IntervalStart, key = true, hidden = true),
        SqlObject("interval"),
        SqlField("site", Mode.Site),
        SqlObject("mode")
      ),

      // TelescopeMode is an inline object in TelescopeModeStatus
      ObjectMapping(TelescopeModeStatusType / "mode")(
        SqlField("_key_site", Mode.Site, key = true, hidden = true),
        SqlField("_key_night", Mode.ObservingNight, key = true, hidden = true),
        SqlField("_key_start", Mode.IntervalStart, key = true, hidden = true),
        SqlField("type", Mode.ModeType),
        SqlField("programReference", Mode.ModeProgramRef)
      )
    ) ++
      // TimestampInterval mappings at each path where an interval appears
      timestampIntervalMappings(TelescopeNightTimelineType / "displayInterval",
                                Timeline.DisplayStart,
                                Timeline.DisplayEnd,
                                Timeline.Site
      ) ++
      timestampIntervalMappings(TelescopeAvailabilityStatusType / "interval",
                                Avail.IntervalStart,
                                Avail.IntervalEnd,
                                Avail.Site
      ) ++
      timestampIntervalMappings(TelescopeTooStatusType / "interval",
                                Too.IntervalStart,
                                Too.IntervalEnd,
                                Too.Site
      ) ++
      timestampIntervalMappings(TelescopeModeStatusType / "interval",
                                Mode.IntervalStart,
                                Mode.IntervalEnd,
                                Mode.Site
      )
