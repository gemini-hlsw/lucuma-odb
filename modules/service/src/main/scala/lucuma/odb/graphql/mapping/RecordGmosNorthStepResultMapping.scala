// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.StepTable

trait RecordGmosNorthStepResultMapping[F[_]] extends StepTable[F] {

  lazy val RecordGmosNorthStepResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = RecordGmosNorthStepResultType,
      fieldMappings = List(
        SqlField("id", StepTable.Id, key = true),
        SqlObject("stepRecord")
      )
    )

}
