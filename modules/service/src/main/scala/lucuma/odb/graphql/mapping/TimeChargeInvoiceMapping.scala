// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.TimeAccountingTable
import lucuma.odb.graphql.table.VisitTable

trait TimeChargeInvoiceMapping[F[_]] extends VisitTable[F] with TimeAccountingTable[F] {

  lazy val TimeChargeInvoiceMapping: ObjectMapping =
    ObjectMapping(
      tpe = TimeChargeInvoiceType,
      fieldMappings = List(
        SqlField("id", VisitTable.Id, key = true, hidden = true),
        SqlObject("executionTime", Join(VisitTable.Id, TimeAccountingTable.VisitId)),
        SqlObject("finalCharge",   Join(VisitTable.Id, TimeAccountingTable.VisitId))
      )
    )
}
