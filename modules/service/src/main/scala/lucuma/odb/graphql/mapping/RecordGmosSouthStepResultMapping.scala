// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.StepTable

trait RecordGmosSouthStepResultMapping[F[_]] extends StepTable[F] {

  lazy val RecordGmosSouthStepResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = RecordGmosSouthStepResultType,
      fieldMappings = List(
        SqlField("id", StepTable.Id, key = true),
        SqlObject("stepRecord")
      )
    )

}
