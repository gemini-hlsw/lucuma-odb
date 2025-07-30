// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.apply.*
import io.circe.syntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.ChronDatasetUpdateView
import lucuma.odb.graphql.table.DatasetTable
import lucuma.odb.graphql.table.UserTable
import lucuma.odb.json.time.query.given

trait DatasetChronicleEntryMapping[F[_]] extends ChronDatasetUpdateView[F]
                                            with DatasetTable[F]
                                            with UserTable[F]:

  lazy val DatasetChronicleEntryMapping: ObjectMapping =
    ObjectMapping(DatasetChronicleEntryType)(
      SqlField("id",               ChronDatasetUpdateView.ChronId, key = true),
      SqlField("transactionId",    ChronDatasetUpdateView.TransactionId),
      SqlObject("user",            Join(ChronDatasetUpdateView.UserId, UserTable.UserId)),
      SqlField("timestamp",        ChronDatasetUpdateView.Timestamp),
      SqlField("operation",        ChronDatasetUpdateView.OperationId),

      SqlObject("dataset",         Join(ChronDatasetUpdateView.DatasetId, DatasetTable.Id)),

      SqlField("modDatasetId",     ChronDatasetUpdateView.Mod.DatasetId),
      SqlField("modStepId",        ChronDatasetUpdateView.Mod.StepId),
      SqlField("modObservationId", ChronDatasetUpdateView.Mod.ObservationId),
      SqlField("modVisitId",       ChronDatasetUpdateView.Mod.VisitId),
      SqlField("modReference",     ChronDatasetUpdateView.Mod.Reference),
      SqlField("modFilename",      ChronDatasetUpdateView.Mod.FileName),
      SqlField("modQaState",       ChronDatasetUpdateView.Mod.QaState),
      SqlField("modInterval",      ChronDatasetUpdateView.Mod.Interval),
      SqlField("modComment",       ChronDatasetUpdateView.Mod.Comment),

      SqlField("newDatasetId",     ChronDatasetUpdateView.New.DatasetId),
      SqlField("newStepId",        ChronDatasetUpdateView.New.StepId),
      SqlField("newObservationId", ChronDatasetUpdateView.New.ObservationId),
      SqlField("newVisitId",       ChronDatasetUpdateView.New.VisitId),

      SqlField("newReference",     ChronDatasetUpdateView.New.DatasetReference),
      SqlField("newFilename",      ChronDatasetUpdateView.New.FileName),
      SqlField("newQaState",       ChronDatasetUpdateView.New.QaState),

      SqlField("start",            ChronDatasetUpdateView.Coalesce.StartTime, hidden = true),
      SqlField("end",              ChronDatasetUpdateView.Coalesce.EndTime, hidden = true),

      CursorFieldJson(
        "newInterval",
        cursor =>
          for
            m <- cursor.fieldAs[Boolean]("modInterval")
            s <- cursor.fieldAs[Option[Timestamp]]("start")
            e <- cursor.fieldAs[Option[Timestamp]]("end")
          yield Option.when(m)((s, e).tupled).flatten.map { (ts, te) => TimestampInterval.between(ts, te) }.asJson,
        List("modInterval", "start", "end")
      ),

      SqlField("newComment",       ChronDatasetUpdateView.New.Comment)
    )