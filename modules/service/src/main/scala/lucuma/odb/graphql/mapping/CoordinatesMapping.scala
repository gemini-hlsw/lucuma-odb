// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ConfigurationRequestView

import table.ObservationView

trait CoordinatesMapping[F[_]] extends ObservationView[F] with ConfigurationRequestView[F] {

  lazy val CoordinatesMappings =
    List(
      CoordinatesMapping,
      ConfigurationRequestReferenceCoordinatesMapping,
    )

  private lazy val CoordinatesMapping =
    ObjectMapping(TargetEnvironmentType / "explicitBase")(
      SqlField("synthetic_id", ObservationView.TargetEnvironment.Coordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

  private lazy val ConfigurationRequestReferenceCoordinatesMapping =
    ObjectMapping(ConfigurationTargetType / "coordinates")(
      SqlField("synthetic-id", ConfigurationRequestView.Target.ReferenceCoordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

}

