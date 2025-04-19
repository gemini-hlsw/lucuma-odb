// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GmosDynamicTables

trait GmosDynamicMapping[F[_]] extends GmosDynamicTables[F]:

  private def dynamicMappingAtPath[G, L, U](
    path: Path,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("id",       table.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlObject("readout"),
      SqlField("dtax",     table.Dtax),
      SqlField("roi",      table.Roi),
      SqlObject("gratingConfig"),
      SqlField("filter",   table.Filter),
      SqlObject("fpu"),
      SqlObject("centralWavelength")
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def dynamicSwitchMapping[G, L, U](
    name:        String,
    table:       GmosDynamicTable[G, L, U]
  ): List[TypeMapping] =
    List(
      dynamicMappingAtPath(StepRecordType / name, table)
    )

  lazy val GmosNorthDynamicMappings: List[TypeMapping] =
    dynamicSwitchMapping("gmosNorth", GmosNorthDynamicTable)

  lazy val GmosSouthDynamicMappings: List[TypeMapping] =
    dynamicSwitchMapping("gmosSouth", GmosSouthDynamicTable)