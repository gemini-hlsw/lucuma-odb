// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.TimeAccountingTable

trait AddTimeChargeCorrectionResultMapping[F[_]] extends TimeAccountingTable[F] {

  lazy val AddTimeChargeCorrectionResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = AddTimeChargeCorrectionResultType,
      fieldMappings = List(
        SqlField("id", TimeAccountingTable.VisitId, key = true, hidden = true),
        SqlObject("timeChargeInvoice")
      )
    )

}

