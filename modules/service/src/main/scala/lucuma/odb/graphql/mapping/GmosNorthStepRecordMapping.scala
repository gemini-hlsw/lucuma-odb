// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.StepTable

trait GmosNorthStepRecordMapping[F[_]] extends StepTable[F] {

  lazy val GmosNorthStepRecordMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosNorthStepRecordType,
      fieldMappings = List(
        SqlField("id", StepTable.Id, key = true),
        // TBD - more fields!
      )
    )

}
