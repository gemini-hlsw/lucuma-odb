// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.StepRecordView

trait RecordGmosStepResultMapping[F[_]] extends StepRecordView[F] {

  private def recordStepResultMapping(
    typeRef: TypeRef
  ): ObjectMapping =
    ObjectMapping(typeRef)(
      SqlField("id", StepRecordView.Id, key = true, hidden = true),
      SqlObject("stepRecord")
    )

  lazy val RecordGmosNorthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosNorthStepResultType)

  lazy val RecordGmosSouthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosSouthStepResultType)

}
