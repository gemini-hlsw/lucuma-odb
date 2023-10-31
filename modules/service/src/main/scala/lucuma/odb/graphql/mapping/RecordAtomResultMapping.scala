// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable

trait RecordAtomResultMapping[F[_]] extends AtomRecordTable[F] {

  lazy val RecordAtomResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = RecordAtomResultType,
      fieldMappings = List(
        SqlField("id", AtomRecordTable.Id, key = true),
        SqlObject("atomRecord")
      )
    )

}
