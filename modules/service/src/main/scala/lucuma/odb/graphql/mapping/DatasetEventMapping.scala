// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.data.OptionT
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Dataset

import java.time.LocalDate

import table.ObservationView
import table.DatasetEventTable
import table.VisitTable

trait DatasetEventMapping[F[_]] extends DatasetEventTable[F]
                                   with ObservationView[F]
                                   with VisitTable[F] {

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetEventType,
      fieldMappings = List(
        SqlField("id",           DatasetEventTable.Id, key = true),
        SqlField("visitId",      DatasetEventTable.VisitId),
        SqlObject("datasetId"),
        SqlField("datasetStage", DatasetEventTable.DatasetStage),

        SqlField("fileSite",     DatasetEventTable.DatasetFilename.FileSite,  hidden = true),
        SqlField("fileDate",     DatasetEventTable.DatasetFilename.FileDate,  hidden = true),
        SqlField("fileIndex",    DatasetEventTable.DatasetFilename.FileIndex, hidden = true),
        CursorField(
          "filename",
          c => {
            (for {
              s <- OptionT(c.fieldAs[Option[Site]]("fileSite"))
              d <- OptionT(c.fieldAs[Option[LocalDate]]("fileDate"))
              i <- OptionT(c.fieldAs[Option[PosInt]]("fileIndex"))
              r <- OptionT.liftF(Result.fromOption(Dataset.Filename.from(s, d, i), s"Invalid filename").map(_.format))
            } yield r).value
          },
          List("fileSite", "fileDate", "fileIndex")
        ),

        SqlObject("observation", Join(DatasetEventTable.VisitId, VisitTable.Id), Join(VisitTable.ObservationId, ObservationView.Id)),
        SqlField("received",     DatasetEventTable.Received)
      )
    )


}
