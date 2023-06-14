// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef
import table.GmosDynamicTables

trait GmosCcdModeMapping[F[_]] extends GmosDynamicTables[F] {

  private def ccdModeMapping[G, L, U](
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = GmosCcdModeType,
      fieldMappings = List(
        SqlField("id", table.Id, key = true, hidden = true),
        SqlField("xBin",        table.CcdMode.Xbin),
        SqlField("yBin",        table.CcdMode.Ybin),
        SqlField("ampCount",    table.CcdMode.AmpCount),
        SqlField("ampGain",     table.CcdMode.AmpGain),
        SqlField("ampReadMode", table.CcdMode.AmpReadMode)
      )
    )

  lazy val GmosCcdModeMapping: TypeMapping =
    SwitchMapping(
      GmosCcdModeType,
      List(
        GmosNorthStepRecordType / "instrumentConfig" / "readout" -> ccdModeMapping(GmosNorthDynamicTable),
        GmosSouthStepRecordType / "instrumentConfig" / "readout" -> ccdModeMapping(GmosSouthDynamicTable)
      )
    )

}
