// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.VisitTable

trait AddTimeChargeCorrectionResultMapping[F[_]] extends VisitTable[F] {

  lazy val AddTimeChargeCorrectionResultMapping: ObjectMapping =
    ObjectMapping(AddTimeChargeCorrectionResultType)(
      SqlField("id", VisitTable.Id, key = true, hidden = true),
      SqlObject("timeChargeInvoice")
    )

}

