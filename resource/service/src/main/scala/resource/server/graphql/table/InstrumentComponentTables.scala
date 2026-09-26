// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.codec.text.varchar

trait InstrumentComponentTables[F[_]] extends BaseMapping[F]:

  object InstrumentComponentTable extends TableDef("t_instrument_component"):
    val Id            = col("c_id", varchar)
    val Instrument    = col("c_instrument", resource_instrument)
    val ComponentType = col("c_component_type", instrument_component_type)
    val Code          = col("c_code", text_nonempty)
    val Name          = col("c_name", text_nonempty)
    val Barcode       = col("c_barcode", text_nonempty.opt)
    val Aliases       = col("c_aliases", nonempty_text_array)
    val Existence     = col("c_existence", existence)

  /** The component catalog per site. See the view's comment for c_search. */
  object InstrumentComponentAtSiteView extends TableDef("v_instrument_component_at_site"):
    val Site          = col("c_site", site)
    val Id            = col("c_id", varchar)
    val Instrument    = col("c_instrument", resource_instrument)
    val ComponentType = col("c_component_type", instrument_component_type)
    val Code          = col("c_code", text_nonempty)
    val Name          = col("c_name", text_nonempty)
    val Barcode       = col("c_barcode", text_nonempty.opt)
    val Aliases       = col("c_aliases", nonempty_text_array)
    val Existence     = col("c_existence", existence)
    val Search        = col("c_search", text)

  object InstrumentComponentBlockTable extends TableDef("t_instrument_component_block"):
    val Id          = col("c_id", int8)
    val Site        = col("c_site", site)
    val Start       = col("c_start", core_timestamp)
    val End         = col("c_end", core_timestamp)
    val ComponentId = col("c_component_id", varchar)
    val Usage       = col("c_usage", resource_usage)
    val Location    = col("c_location", component_location)
    val Note        = col("c_note", text_nonempty.opt)
