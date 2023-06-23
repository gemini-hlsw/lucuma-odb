// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.GmosDynamicTables

trait GmosCustomMaskMapping[F[_]] extends GmosDynamicTables[F] {

  private def customMaskMapping[G, L, U](
    table: GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = GmosCustomMaskType,
      fieldMappings = List(
        SqlField("synthetic_id", table.Fpu.CustomMask.SyntheticId, key = true, hidden = true),
        SqlField("filename",     table.Fpu.CustomMask.Filename,  hidden = true),
        SqlField("slitWidth",    table.Fpu.CustomMask.SlitWidth, hidden = true)
      )
    )

  lazy val GmosCustomMaskMapping: TypeMapping =
    SwitchMapping(
      GmosCustomMaskType,
      List(
        GmosNorthStepRecordType / "instrumentConfig" / "fpu" / "customMask" -> customMaskMapping(GmosNorthDynamicTable),
        GmosSouthStepRecordType / "instrumentConfig" / "fpu" / "customMask" -> customMaskMapping(GmosSouthDynamicTable)
      )
    )

}
