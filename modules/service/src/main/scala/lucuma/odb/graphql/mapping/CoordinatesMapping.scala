// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GhostIfuView
import lucuma.odb.graphql.table.ArchiveDuplicationView
import lucuma.odb.graphql.table.ArchiveMatchView

import table.ObservationView

trait CoordinatesMapping[F[_]] extends ObservationView[F]
                                  with ConfigurationRequestView[F]
                                  with GhostIfuView[F]
                                  with ArchiveDuplicationView[F]
                                  with ArchiveMatchView[F]:

  lazy val CoordinatesMappings =
    List(
      CoordinatesMapping,
      ConfigurationRequestReferenceCoordinatesMapping,
      GhostIfuSkyPositionMapping,
      ArchiveDuplicationSearchCoordinatesMapping,
      ArchiveMatchCoordinatesMapping
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

  private lazy val ArchiveDuplicationSearchCoordinatesMapping =
    ObjectMapping(ArchiveDuplicationType / "searchCoordinates")(
      SqlField("synthetic-id", ArchiveDuplicationView.SearchCoordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

  private lazy val ArchiveMatchCoordinatesMapping =
    ObjectMapping(ArchiveMatchType / "coordinates")(
      SqlField("synthetic-id", ArchiveMatchView.Coordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

  private lazy val GhostIfuSkyPositionMapping =
    ObjectMapping(GhostIfuType / "skyPosition")(
      SqlField("synthetic-id", GhostIfuView.Sky.Id, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )