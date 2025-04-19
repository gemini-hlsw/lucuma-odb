// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.table.TimeChargeCorrectionTable
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.VisitTable

trait TimeChargeInvoiceMapping[F[_]] extends VisitTable[F]
                                        with TimeChargeCorrectionTable[F]
                                        with TimeChargeDiscountTable[F]:

  lazy val TimeChargeInvoiceMapping: ObjectMapping =
    ObjectMapping(TimeChargeInvoiceType)(
      SqlField("id", VisitTable.Id, key = true, hidden = true),
      SqlObject("executionTime"),
      SqlObject("discounts",     Join(VisitTable.Id, TimeChargeDiscountTable.VisitId)),
      SqlObject("corrections",   Join(VisitTable.Id, TimeChargeCorrectionTable.VisitId)),
      SqlObject("finalCharge")
    )

  lazy val TimeChargeInvoiceElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (TimeChargeInvoiceType, "discounts", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[Timestamp](TimeChargeDiscountType / "start"))), child)

    case (TimeChargeInvoiceType, "corrections", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[Timestamp](TimeChargeCorrectionType / "created"))), child)