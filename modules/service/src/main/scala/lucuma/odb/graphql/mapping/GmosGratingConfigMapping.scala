// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.GmosDynamicTables

trait GmosGratingConfigMapping[F[_]] extends GmosDynamicTables[F] {

  private def gratingMappingAtPath[G, L, U](
    path: Path,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", table.Grating.SyntheticId, key = true, hidden = true),
      SqlField("grating",      table.Grating.Disperser),
      SqlField("order",        table.Grating.Order),
      SqlObject("wavelength")
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def gratingSwitchMapping[G, L, U](
    name:              String,
    table:             GmosDynamicTable[G, L, U]
  ): List[TypeMapping] =
    List(
      gratingMappingAtPath(StepRecordType / name / "gratingConfig", table)
    )

  lazy val GmosNorthGratingConfigMappings: List[TypeMapping] =
    gratingSwitchMapping("gmosNorth", GmosNorthDynamicTable)

  lazy val GmosSouthGratingConfigMappings: List[TypeMapping] =
    gratingSwitchMapping("gmosSouth", GmosSouthDynamicTable)

}
