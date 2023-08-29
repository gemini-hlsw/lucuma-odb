// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef

import table.GmosDynamicTables
import table.StepTable

trait GmosStepRecordMapping[F[_]] extends StepTable[F] with GmosDynamicTables[F] {

  private def stepRecordMapping[G, L, U](
    typeRef: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", StepTable.Id, key = true),
        SqlField("atomId", StepTable.AtomId),
        SqlField("stepIndex", StepTable.StepIndex),

        SqlObject("instrumentConfig", Join(StepTable.Id, table.Id)),
        SqlObject("stepConfig")

        // TBD - more fields!
      )
    )

  lazy val GmosNorthStepRecordMapping: ObjectMapping =
    stepRecordMapping(GmosNorthStepRecordType, GmosNorthDynamicTable)

  lazy val GmosSouthStepRecordMapping: ObjectMapping =
    stepRecordMapping(GmosSouthStepRecordType, GmosSouthDynamicTable)

}
