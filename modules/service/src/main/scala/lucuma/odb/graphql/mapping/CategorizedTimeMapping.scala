// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import io.circe.syntax.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.TimeAccountingTable
import lucuma.odb.json.time.query.given

trait CategorizedTimeMapping[F[_]] extends TimeAccountingTable[F] {

  lazy val CategorizedTimeMapping: TypeMapping =
    SwitchMapping(
      CategorizedTimeType,
      List(
        TimeChargeInvoiceType / "executionTime" ->
          categorizedTimeMapping(
            TimeAccountingTable.VisitId,
            TimeAccountingTable.Raw.NonChargedTime,
            TimeAccountingTable.Raw.PartnerTime,
            TimeAccountingTable.Raw.ProgramTime
          ),

        TimeChargeInvoiceType / "finalCharge"   ->
          categorizedTimeMapping(
            TimeAccountingTable.VisitId,
            TimeAccountingTable.Final.NonChargedTime,
            TimeAccountingTable.Final.PartnerTime,
            TimeAccountingTable.Final.ProgramTime
          )
      )
    )

  private def categorizedTimeMapping(
    key:        ColumnRef,
    nonCharged: ColumnRef,
    partner:    ColumnRef,
    program:    ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = CategorizedTimeType,
      fieldMappings = List(
        SqlField(s"key", key, key = true, hidden = true),

        SqlObject("nonCharged"),
        SqlField("nonChargedTs", nonCharged, hidden = true),

        SqlObject("partner"),
        SqlField("partnerTs", partner, hidden = true),

        SqlObject("program"),
        SqlField("programTs", program, hidden = true),

        CursorFieldJson(
          "total",
          cursor =>
            for {
              n   <- cursor.field("nonChargedTs", None)
              nts <- n.as[TimeSpan]
              a   <- cursor.field("partnerTs", None)
              ats <- a.as[TimeSpan]
              r   <- cursor.field("programTs", None)
              rts <- r.as[TimeSpan]
            } yield (nts +| ats +| rts).asJson,
          List("nonChargedTs", "partnerTs", "programTs")
        )
      )
    )

}
