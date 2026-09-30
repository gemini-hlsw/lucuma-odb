// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GhostIfuView

trait ConfigurationGhostIfuMappings[F[_]]
  extends GhostIfuView[F]
     with ConfigurationRequestView[F]:

  lazy val ConfigurationGhostIfuMappings = List(
    ConfigurationGhostIfuMapping,
    ConfigurationRequestGhostIfuMapping,
  )

  private lazy val ConfigurationGhostIfuMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "ghostIfu")(
      SqlField("observationId", GhostIfuView.ObservationId, key = true, hidden = true),
      SqlField("resolutionMode", GhostIfuView.ResolutionMode),
    )

  private lazy val ConfigurationRequestGhostIfuMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "ghostIfu")(
      SqlField("id", ConfigurationRequestView.GhostIfu.Id, key = true, hidden = true),
      SqlField("resolutionMode", ConfigurationRequestView.GhostIfu.ResolutionMode),
    )
