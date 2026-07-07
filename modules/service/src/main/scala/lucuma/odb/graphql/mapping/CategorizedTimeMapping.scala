// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import io.circe.syntax.*
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.VisitTable
import lucuma.odb.json.time.query.given

trait CategorizedTimeMapping[F[_]] extends ObservationView[F] with VisitTable[F]:

  lazy val CategorizedTimeMappings: List[TypeMapping] =
    List(
      categorizedTimeMappingAtPath(
        TimeChargeInvoiceType / "executionTime",
        VisitTable.Id,
        VisitTable.Raw.NonChargedTime,
        VisitTable.Raw.ProgramTime
      ),

      categorizedTimeMappingAtPath(
        TimeChargeInvoiceType / "finalCharge",
        VisitTable.Id,
        VisitTable.Final.NonChargedTime,
        VisitTable.Final.ProgramTime
      ),

      categorizedTimeMappingAtPath(
        ExecutionType / "originalEstimate" / "science",
        ObservationView.OriginalEstimate.SyntheticId,
        ObservationView.OriginalEstimate.SciNonChargedTime,
        ObservationView.OriginalEstimate.SciProgramTime
      ),

      categorizedTimeMappingAtPath(
        ExecutionType / "originalEstimate" / "fullTimeEstimate",
        ObservationView.OriginalEstimate.SyntheticId,
        ObservationView.OriginalEstimate.FullNonChargedTime,
        ObservationView.OriginalEstimate.FullProgramTime
      )
    )

  private def categorizedTimeMappingAtPath(
    path:       Path,
    key:        ColumnRef,
    nonCharged: ColumnRef,
    program:    ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField(s"key", key, key = true, hidden = true),

      SqlObject("nonCharged"),
      SqlField("nonChargedTs", nonCharged, hidden = true),

      SqlObject("program"),
      SqlField("programTs", program, hidden = true),

      CursorFieldJson(
        "total",
        cursor =>
          for {
            n   <- cursor.field("nonChargedTs", None)
            nts <- n.as[TimeSpan]
            r   <- cursor.field("programTs", None)
            rts <- r.as[TimeSpan]
          } yield (nts +| rts).asJson,
        List("nonChargedTs", "programTs")
      )
    )