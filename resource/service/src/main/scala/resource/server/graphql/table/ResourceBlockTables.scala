// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.numeric.int8

trait ResourceBlockTables[F[_]] extends BaseMapping[F]:

  object TooSupportBlockTable extends TableDef("t_too_support_block"):
    val Id         = col("c_id", int8)
    val Site       = col("c_site", site)
    val Start      = col("c_start", core_timestamp)
    val End        = col("c_end", core_timestamp)
    val TooSupport = col("c_too_support", too_support)
    val Note       = col("c_note", text_nonempty.opt)

  object TelescopeAvailabilityBlockTable extends TableDef("t_telescope_availability_block"):
    val Id           = col("c_id", int8)
    val Site         = col("c_site", site)
    val Start        = col("c_start", core_timestamp)
    val End          = col("c_end", core_timestamp)
    val Availability = col("c_availability", telescope_availability)
    val Port         = col("c_port", int4_pos.opt)
    val Reason       = col("c_reason", text_nonempty.opt)
    val Note         = col("c_note", text_nonempty.opt)

  object TelescopeModeBlockTable extends TableDef("t_telescope_mode_block"):
    val Id                = col("c_id", int8)
    val Site              = col("c_site", site)
    val Start             = col("c_start", core_timestamp)
    val End               = col("c_end", core_timestamp)
    val Mode              = col("c_mode", telescope_mode_type)
    val ProgramReferences = col("c_program_references", program_reference_array)
    val Partner           = col("c_partner", partner.opt)
    val Note              = col("c_note", text_nonempty.opt)

  object InstrumentAvailabilityBlockTable extends TableDef("t_instrument_availability_block"):
    val Id            = col("c_id", int8)
    val Site          = col("c_site", site)
    val Start         = col("c_start", core_timestamp)
    val End           = col("c_end", core_timestamp)
    val Instrument    = col("c_instrument", resource_instrument)
    val PublishedName = col("c_published_name", text_nonempty)
    val Place         = col("c_place", instrument_place)
    val Port          = col("c_port", int4_pos.opt)
    val Usage         = col("c_usage", resource_usage)
    val Note          = col("c_note", text_nonempty.opt)

  object TelescopeSubsystemBlockTable extends TableDef("t_telescope_subsystem_block"):
    val Id          = col("c_id", int8)
    val Site        = col("c_site", site)
    val Start       = col("c_start", core_timestamp)
    val End         = col("c_end", core_timestamp)
    val Subsystem   = col("c_subsystem", telescope_subsystem)
    val Usage       = col("c_usage", resource_usage)
    val PowerSource = col("c_power_source", power_source.opt)
    val Note        = col("c_note", text_nonempty.opt)
