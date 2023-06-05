// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.SequenceEventTable

trait AddSequenceEventResultMapping[F[_]] extends SequenceEventTable[F] {

  lazy val AddSequenceEventResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = AddSequenceEventResultType,
      fieldMappings = List(
        SqlField("id", SequenceEventTable.Id, key = true),
        SqlObject("event")
      )
    )

}
