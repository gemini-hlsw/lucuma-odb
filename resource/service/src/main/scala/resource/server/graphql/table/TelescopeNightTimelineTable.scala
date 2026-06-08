// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.temporal.date
import skunk.codec.text.text

trait TelescopeNightTimelineTable[F[_]] extends BaseMapping[F]:

  object TelescopeNightTimelineTable extends TableDef("t_telescope_night_timeline"):
    val Site           = col("c_site", site)
    val ObservingNight = col("c_observing_night", date)
    val DisplayStart   = col("c_display_interval_start", core_timestamp)
    val DisplayEnd     = col("c_display_interval_end", core_timestamp)

  object TelescopeAvailabilityStatusTable extends TableDef("t_telescope_availability_status"):
    val Site                = col("c_site", site)
    val ObservingNight      = col("c_observing_night", date)
    val IntervalStart       = col("c_interval_start", core_timestamp)
    val IntervalEnd         = col("c_interval_end", core_timestamp)
    val Availability        = col("c_availability", telescope_availability)
    val Reason              = col("c_reason", text.opt)
    val PlannedAvailability = col("c_planned_availability", telescope_availability.opt)

  object TelescopeTooStatusTable extends TableDef("t_telescope_too_status"):
    val Site           = col("c_site", site)
    val ObservingNight = col("c_observing_night", date)
    val IntervalStart  = col("c_interval_start", core_timestamp)
    val IntervalEnd    = col("c_interval_end", core_timestamp)
    val TooSupport     = col("c_too_support", too_support)

  object TelescopeModeStatusTable extends TableDef("t_telescope_mode_status"):
    val Site           = col("c_site", site)
    val ObservingNight = col("c_observing_night", date)
    val IntervalStart  = col("c_interval_start", core_timestamp)
    val IntervalEnd    = col("c_interval_end", core_timestamp)
    val ModeType       = col("c_mode_type", telescope_mode_type)
    val ModeProgramRef = col("c_mode_program_reference", program_reference.opt)
