// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GhostIfuView
import lucuma.odb.graphql.table.GoaDuplicationView

import table.ObservationView

trait CoordinatesMapping[F[_]] extends ObservationView[F]
                                  with ConfigurationRequestView[F]
                                  with GhostIfuView[F]
                                  with GoaDuplicationView[F]:

  lazy val CoordinatesMappings =
    List(
      CoordinatesMapping,
      ConfigurationRequestReferenceCoordinatesMapping,
      GhostIfuSkyPositionMapping,
      GoaDuplicationSearchCoordinatesMapping
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

  private lazy val GoaDuplicationSearchCoordinatesMapping =
    ObjectMapping(GoaDuplicationType / "searchCoordinates")(
      SqlField("synthetic-id", GoaDuplicationView.SearchCoordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

  private lazy val GhostIfuSkyPositionMapping =
    ObjectMapping(GhostIfuType / "skyPosition")(
      SqlField("synthetic-id", GhostIfuView.Sky.Id, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )