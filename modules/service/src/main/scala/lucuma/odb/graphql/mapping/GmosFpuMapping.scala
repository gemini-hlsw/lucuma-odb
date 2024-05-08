// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GmosDynamicTables

trait GmosFpuMapping[F[_]] extends GmosDynamicTables[F] {

  private def fpuMappingAtPath[G, L, U](
    path: Path,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(PathMatch(path))(
      SqlField("synthetic_id", table.Fpu.SyntheticId, key = true, hidden = true),
      SqlObject("customMask"),
      SqlField("builtin", table.Fpu.Builtin)
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def fpuSwitchMapping[G, L, U](
    name:    String,
    table:   GmosDynamicTable[G, L, U]
  ): List[TypeMapping] =
    List(
      fpuMappingAtPath( StepRecordType / name / "fpu", table)
    )

  lazy val GmosNorthFpuMappings: List[TypeMapping] =
    fpuSwitchMapping("gmosNorth", GmosNorthDynamicTable)


  lazy val GmosSouthFpuMappings: List[TypeMapping] =
    fpuSwitchMapping("gmosSouth", GmosSouthDynamicTable)


}
