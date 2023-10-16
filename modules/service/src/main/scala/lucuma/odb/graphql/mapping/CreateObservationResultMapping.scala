// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ObservationView

trait CreateObservationResultMapping[F[_]] extends ObservationView[F] {

  lazy val CreateObservationResultMapping =
    ObjectMapping(
      tpe = CreateObservationResultType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlObject("observation"),
      )
    )

}

