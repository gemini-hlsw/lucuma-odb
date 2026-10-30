// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.apply.*
import cats.syntax.order.*
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.TimeChargeCorrectionTable
import lucuma.odb.graphql.table.TimeChargeDiscountTable
import lucuma.odb.graphql.table.VisitTable
import lucuma.odb.json.time.query.given

trait TimeChargeInvoiceMapping[F[_]] extends VisitTable[F]
                                        with TimeChargeCorrectionTable[F]
                                        with TimeChargeDiscountTable[F]:

  lazy val TimeChargeInvoiceMapping: ObjectMapping =
    ObjectMapping(TimeChargeInvoiceType)(
      SqlField("id", VisitTable.Id, key = true, hidden = true),
      SqlField("_start",      VisitTable.Start,      hidden = true),
      SqlField("_end",        VisitTable.End,        hidden = true),
      SqlField("_visitorEnd", VisitTable.VisitorEnd, hidden = true),

      // The execution time is measured from the first event to the last or,
      // for a visitor instrument visit, to where its charge ends (see
      // TimeAccountingService.VisitInfo.visitorEnd).
      CursorFieldJson(
        "interval",
        cursor =>
          for
            s <- cursor.fieldAs[Option[Timestamp]]("_start")
            e <- cursor.fieldAs[Option[Timestamp]]("_end")
            v <- cursor.fieldAs[Option[Timestamp]]("_visitorEnd")
          yield (s, e).mapN((ts, te) => TimestampInterval.between(ts, v.fold(te)(_ max te))).asJson,
        List("_start", "_end", "_visitorEnd")
      ),

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