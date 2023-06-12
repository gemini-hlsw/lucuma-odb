// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.StepTable

trait GmosSouthStepRecordMapping[F[_]] extends StepTable[F] {

  lazy val GmosSouthStepRecordMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosSouthStepRecordType,
      fieldMappings = List(
        SqlField("id", StepTable.Id, key = true),
        // TBD - more fields!
      )
    )

}
