// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ObservationView
import table.SequenceEventTable
import table.VisitTable

trait SequenceEventMapping[F[_]] extends SequenceEventTable[F]
                                    with ObservationView[F]
                                    with VisitTable[F] {

  lazy val SequenceEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = SequenceEventType,
      fieldMappings = List(
        SqlField("id",           SequenceEventTable.Id, key = true),
        SqlField("visitId",      SequenceEventTable.VisitId),
        SqlField("command",      SequenceEventTable.SequenceCommand),
        SqlObject("observation", Join(SequenceEventTable.VisitId, VisitTable.Id), Join(VisitTable.ObservationId, ObservationView.Id)),
        SqlField("received",     SequenceEventTable.Received)
      )
    )

}