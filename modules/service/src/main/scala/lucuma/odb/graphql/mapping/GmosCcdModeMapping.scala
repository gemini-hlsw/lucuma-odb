// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GmosDynamicTables

trait GmosCcdModeMapping[F[_]] extends GmosDynamicTables[F] {

  private def ccdModeMappingAtPath[G, L, U](
    path: Path,
    table: GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("id", table.Id, key = true, hidden = true),
      SqlField("xBin",        table.CcdMode.Xbin),
      SqlField("yBin",        table.CcdMode.Ybin),
      SqlField("ampCount",    table.CcdMode.AmpCount),
      SqlField("ampGain",     table.CcdMode.AmpGain),
      SqlField("ampReadMode", table.CcdMode.AmpReadMode)
    )

  lazy val GmosCcdModeMappings: List[TypeMapping] =
    List(
      ccdModeMappingAtPath(StepRecordType / "gmosNorth" / "readout", GmosNorthDynamicTable),
      ccdModeMappingAtPath(StepRecordType / "gmosSouth" / "readout", GmosSouthDynamicTable),
      ccdModeMappingAtPath(GmosNorthStepType / "instrumentConfig" / "readout", GmosNorthDynamicTable),
      ccdModeMappingAtPath(GmosSouthStepType / "instrumentConfig" / "readout", GmosSouthDynamicTable)
    )

}
