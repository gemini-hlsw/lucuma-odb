// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.AtomRecordTable
import table.StepRecordTable
import table.VisitTable

trait GmosAtomRecordMapping[F[_]] extends AtomRecordTable[F]
                                     with StepRecordTable[F]
                                     with VisitTable[F] {

  private def atomRecordMapping(typeRef: TypeRef): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", AtomRecordTable.Id, key = true),
        SqlObject("visit", Join(AtomRecordTable.VisitId, VisitTable.Id)),
        SqlField("sequenceType", AtomRecordTable.SequenceType),
        SqlField("stepCount", AtomRecordTable.StepCount),

        SqlObject("steps", Join(AtomRecordTable.Id, StepRecordTable.AtomId))

        // TBD - more fields!
      )
    )

  lazy val GmosNorthAtomRecordMapping: ObjectMapping =
    atomRecordMapping(GmosNorthAtomRecordType)

  lazy val GmosSouthAtomRecordMapping: ObjectMapping =
    atomRecordMapping(GmosSouthAtomRecordType)

}
