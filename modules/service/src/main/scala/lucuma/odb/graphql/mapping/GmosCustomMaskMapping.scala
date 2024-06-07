// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GmosDynamicTables

trait GmosCustomMaskMapping[F[_]] extends GmosDynamicTables[F] {

  private def customMaskMappingAtPath[G, L, U](
    path: Path,
    table: GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", table.Fpu.CustomMask.SyntheticId, key = true, hidden = true),
      SqlField("filename",     table.Fpu.CustomMask.Filename),
      SqlField("slitWidth",    table.Fpu.CustomMask.SlitWidth)
    )

  lazy val GmosCustomMaskMapping: List[TypeMapping] =
    List(
      customMaskMappingAtPath(StepRecordType / "gmosNorth" / "fpu" / "customMask", GmosNorthDynamicTable),
      customMaskMappingAtPath(StepRecordType / "gmosSouth" / "fpu" / "customMask", GmosSouthDynamicTable),
      customMaskMappingAtPath(GmosNorthStepType / "instrumentConfig" / "fpu" / "customMask", GmosNorthDynamicTable),
      customMaskMappingAtPath(GmosSouthStepType / "instrumentConfig" / "fpu" / "customMask", GmosSouthDynamicTable)
    )

}
