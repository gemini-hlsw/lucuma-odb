// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import io.circe.syntax.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.VisitTable
import lucuma.odb.json.time.query.given

trait CategorizedTimeMapping[F[_]] extends VisitTable[F] {

  lazy val CategorizedTimeMappings: List[TypeMapping] =
    List(
        categorizedTimeMappingAtPath(
          TimeChargeInvoiceType / "executionTime",      
          VisitTable.Id,
          VisitTable.Raw.NonChargedTime,
          VisitTable.Raw.PartnerTime,
          VisitTable.Raw.ProgramTime
        ),

        categorizedTimeMappingAtPath(
          TimeChargeInvoiceType / "finalCharge",      
          VisitTable.Id,
          VisitTable.Final.NonChargedTime,
          VisitTable.Final.PartnerTime,
          VisitTable.Final.ProgramTime
        )
      )

  private def categorizedTimeMappingAtPath(
    path:       Path,
    key:        ColumnRef,
    nonCharged: ColumnRef,
    partner:    ColumnRef,
    program:    ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
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

}
