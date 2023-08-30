// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef

import table.AtomRecordTable
import table.StepRecordTable

trait GmosAtomRecordMapping[F[_]] extends AtomRecordTable[F] with StepRecordTable[F] {

  private def stepRecordMapping(typeRef: TypeRef): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", AtomRecordTable.Id, key = true),
        SqlField("visitId", AtomRecordTable.VisitId),
        SqlField("sequenceType", AtomRecordTable.SequenceType),
        SqlField("stepCount", AtomRecordTable.StepCount),

        SqlObject("steps", Join(AtomRecordTable.Id, StepRecordTable.AtomId))

        // TBD - more fields!
      )
    )

  lazy val GmosNorthAtomRecordMapping: ObjectMapping =
    stepRecordMapping(GmosNorthAtomRecordType)

  lazy val GmosSouthAtomRecordMapping: ObjectMapping =
    stepRecordMapping(GmosSouthAtomRecordType)

}
