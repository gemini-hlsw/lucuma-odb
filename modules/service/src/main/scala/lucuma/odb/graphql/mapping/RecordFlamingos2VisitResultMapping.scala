// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.VisitTable

 trait RecordFlamingos2VisitResultMapping[F[_]] extends VisitTable[F] {

   lazy val RecordFlamingos2VisitResultMapping: ObjectMapping =
      ObjectMapping(RecordFlamingos2VisitResultType)(
        SqlField("id", VisitTable.Id, key = true, hidden = true),
        SqlObject("visit")
      )

 }
