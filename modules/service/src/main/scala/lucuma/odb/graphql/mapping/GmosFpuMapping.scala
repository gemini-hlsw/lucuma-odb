// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.GmosDynamicTables

trait GmosFpuMapping[F[_]] extends GmosDynamicTables[F] {

  private def fpuMapping[G, L, U](
    typeRef: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("synthetic_id", table.Fpu.SyntheticId, key = true, hidden = true),
        SqlObject("customMask"),
        SqlField("builtin", table.Fpu.Builtin)
      )
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def fpuSwitchMapping[G, L, U](
    name:    String,
    fpuType: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): List[TypeMapping] =
    SwitchMapping(
      fpuType,
      List(
        StepRecordType / name / "fpu" -> fpuMapping(fpuType, table)
      )
    )

  lazy val GmosNorthFpuMappings: List[TypeMapping] =
    fpuSwitchMapping("gmosNorth", GmosNorthFpuType, GmosNorthDynamicTable)


  lazy val GmosSouthFpuMappings: List[TypeMapping] =
    fpuSwitchMapping("gmosSouth", GmosSouthFpuType, GmosSouthDynamicTable)


}
