// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.StepRecordTable

trait RecordGmosStepResultMapping[F[_]] extends StepRecordTable[F] {

  private def recordStepResultMapping(
    typeRef: TypeRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id", StepRecordTable.Id, key = true),
        SqlObject("stepRecord")
      )
    )

  lazy val RecordGmosNorthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosNorthStepResultType)

  lazy val RecordGmosSouthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosSouthStepResultType)

}
