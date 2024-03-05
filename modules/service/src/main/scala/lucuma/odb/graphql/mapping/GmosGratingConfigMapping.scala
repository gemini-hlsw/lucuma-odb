// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.GmosDynamicTables

trait GmosGratingConfigMapping[F[_]] extends GmosDynamicTables[F] {

  private def gratingMapping[G, L, U](
    typeRef: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("synthetic_id", table.Grating.SyntheticId, key = true, hidden = true),
        SqlField("grating",      table.Grating.Disperser),
        SqlField("order",        table.Grating.Order),
        SqlObject("wavelength")
      )
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def gratingSwitchMapping[G, L, U](
    name:              String,
    gratingConfigType: TypeRef,
    table:             GmosDynamicTable[G, L, U]
  ): TypeMapping =
    SwitchMapping(
      gratingConfigType,
      List(
        StepRecordType / name / "gratingConfig" -> gratingMapping(gratingConfigType, table)
      )
    )

  lazy val GmosNorthGratingConfigMapping: TypeMapping =
    gratingSwitchMapping("gmosNorth", GmosNorthGratingConfigType, GmosNorthDynamicTable)

  lazy val GmosSouthGratingConfigMapping: TypeMapping =
    gratingSwitchMapping("gmosSouth", GmosSouthGratingConfigType, GmosSouthDynamicTable)

}
