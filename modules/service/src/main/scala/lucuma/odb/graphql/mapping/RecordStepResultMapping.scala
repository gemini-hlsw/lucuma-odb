// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef

import table.StepView

trait RecordStepResultMapping[F[_]] extends StepView[F]:

  private def recordStepResultMapping(
    typeRef: TypeRef
  ): ObjectMapping =
    ObjectMapping(typeRef)(
      SqlField("id", StepView.Id, key = true, hidden = true),
      SqlObject("stepRecord")
    )

  lazy val RecordFlamingos2StepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordFlamingos2StepResultType)

  lazy val RecordGmosNorthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosNorthStepResultType)

  lazy val RecordGmosSouthStepResultMapping: ObjectMapping =
    recordStepResultMapping(RecordGmosSouthStepResultType)