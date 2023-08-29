// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef

import table.AtomTable
import table.StepTable

trait GmosAtomRecordMapping[F[_]] extends AtomTable[F] with StepTable[F] {

  private def stepRecordMapping(typeRef: TypeRef): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", AtomTable.Id, key = true),
        SqlField("visitId", AtomTable.VisitId),
        SqlField("sequenceType", AtomTable.SequenceType),
        SqlField("stepCount", AtomTable.StepCount),

        SqlObject("steps", Join(AtomTable.Id, StepTable.AtomId))

        // TBD - more fields!
      )
    )

  lazy val GmosNorthAtomRecordMapping: ObjectMapping =
    stepRecordMapping(GmosNorthAtomRecordType)

  lazy val GmosSouthAtomRecordMapping: ObjectMapping =
    stepRecordMapping(GmosSouthAtomRecordType)

}
