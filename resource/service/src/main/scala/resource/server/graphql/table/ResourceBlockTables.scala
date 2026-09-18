// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.numeric.int8
import skunk.codec.text.varchar

trait ResourceBlockTables[F[_]] extends BaseMapping[F]:

  /** The columns every block table shares. */
  abstract class BlockTable(name: String) extends TableDef(name):
    val Id    = col("c_id", int8)
    val Site  = col("c_site", site)
    val Start = col("c_start", core_timestamp)
    val End   = col("c_end", core_timestamp)
    val Note  = col("c_note", text_nonempty.opt)

  object TooSupportBlockTable extends BlockTable("t_too_support_block"):
    val TooSupport = col("c_too_support", too_support)

  object TelescopeAvailabilityBlockTable extends BlockTable("t_telescope_availability_block"):
    val Availability = col("c_availability", telescope_availability)
    val Port         = col("c_port", int4_pos.opt)
    val Reason       = col("c_reason", text_nonempty.opt)

  object TelescopeModeBlockTable extends BlockTable("t_telescope_mode_block"):
    val Mode              = col("c_mode", telescope_mode_type)
    val ProgramReferences = col("c_program_references", program_reference_array)
    val Partner           = col("c_partner", partner.opt)

  object InstrumentAvailabilityBlockTable extends BlockTable("t_instrument_availability_block"):
    val Instrument    = col("c_instrument", resource_instrument)
    val PublishedName = col("c_published_name", text_nonempty)
    val Place         = col("c_place", instrument_place)
    val Port          = col("c_port", int4_pos.opt)
    val Usage         = col("c_usage", resource_usage)

  object TelescopeSubsystemBlockTable extends BlockTable("t_telescope_subsystem_block"):
    val Subsystem   = col("c_subsystem", telescope_subsystem)
    val Usage       = col("c_usage", resource_usage)
    val PowerSource = col("c_power_source", power_source.opt)

  object InstrumentComponentBlockTable extends BlockTable("t_instrument_component_block"):
    val ComponentId = col("c_component_id", varchar)
    val Usage       = col("c_usage", resource_usage)
    val Location    = col("c_location", component_location)
