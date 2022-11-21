// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import io.circe.Json
import lucuma.odb.graphql.table.ObservationView


// This is an experiment.  It seems like this shouldn't be needed and in any
// case this is doesn't work.
trait ItcSuccessMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val ItcSuccessMapping: ObjectMapping =
    ObjectMapping(
      tpe = ItcSuccessType,
      fieldMappings = List(
        SqlField("observationId", ObservationView.Id, key = true, hidden = true),
        CursorFieldJson("exposureTime", _.fieldAs[Json]("exposureTime"), Nil),
        CursorFieldJson("exposures", _.fieldAs[Json]("exposures"), Nil),
        CursorFieldJson("signalToNoise", _.fieldAs[Json]("signalToNoise"), Nil)
      )
    )

}
