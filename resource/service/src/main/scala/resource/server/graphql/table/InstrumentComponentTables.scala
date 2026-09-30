// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.text.text
import skunk.codec.text.varchar

trait InstrumentComponentTables[F[_]] extends BaseMapping[F]:

  /**
   * The columns of a component, as `t_instrument_component` and its per-site view both carry them.
   */
  abstract class ComponentColumns(name: String) extends TableDef(name):
    val Id            = col("c_id", varchar)
    val Instrument    = col("c_instrument", resource_instrument)
    val ComponentType = col("c_component_type", instrument_component_type)
    val Code          = col("c_code", text_nonempty)
    val Name          = col("c_name", text_nonempty)
    val Barcode       = col("c_barcode", text_nonempty.opt)
    val Aliases       = col("c_aliases", nonempty_text_array)
    val Existence     = col("c_existence", existence)

  /** The InstrumentComponent fields, from either source of component columns. */
  protected def componentFields(t: ComponentColumns): List[FieldMapping] =
    List(
      SqlField("id", t.Id, key = true),
      SqlField("instrument", t.Instrument),
      SqlField("componentType", t.ComponentType),
      SqlField("code", t.Code),
      SqlField("name", t.Name),
      SqlField("barcode", t.Barcode),
      SqlField("aliases", t.Aliases),
      SqlField("existence", t.Existence)
    )

  /** The component catalog itself. A block joins it by component id alone. */
  object InstrumentComponentTable extends ComponentColumns("t_instrument_component")

  /**
   * The component catalog per site: one row per (site, component) with at least one block at that
   * site. Only the `components` root reads it. See the view's comment for c_search.
   */
  object InstrumentComponentAtSiteView extends ComponentColumns("v_instrument_component_at_site"):
    val Site   = col("c_site", site)
    val Search = col("c_search", text)
