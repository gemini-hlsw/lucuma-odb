// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
 package mapping

 import table.VisitTable

 trait RecordGmosSouthVisitResultMapping[F[_]] extends VisitTable[F] {

   lazy val RecordGmosSouthVisitResultMapping: ObjectMapping =
      ObjectMapping(RecordGmosSouthVisitResultType)(
        SqlField("id", VisitTable.Id, key = true, hidden = true),
        SqlObject("visit")
      )

 }