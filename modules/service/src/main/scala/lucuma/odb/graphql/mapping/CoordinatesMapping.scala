// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ObservationView
import lucuma.odb.graphql.table.ConfigurationRequestTable

trait CoordinatesMapping[F[_]] extends ObservationView[F] with ConfigurationRequestTable[F] {

  lazy val CoordinatesMappings =
    List(
      CoordinatesMapping,
      ConfigurationRequestCoordinatesMapping,
    )

  private lazy val CoordinatesMapping =
    ObjectMapping(CoordinatesType)(
      SqlField("synthetic_id", ObservationView.TargetEnvironment.Coordinates.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

  private lazy val ConfigurationRequestCoordinatesMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "referenceCoordinates")(
      SqlField("synthetic-id", ConfigurationRequestTable.Id, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec")
    )

}

