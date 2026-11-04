// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GnirsImagingView

trait ConfigurationGnirsImagingMappings[F[_]]
  extends GnirsImagingView[F]
     with ConfigurationRequestView[F]:

  lazy val ConfigurationGnirsImagingMappings = List(
    ConfigurationGnirsImagingMapping,
    ConfigurationRequestGnirsImagingMapping,
  )

  private lazy val ConfigurationGnirsImagingMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gnirsImaging")(
      SqlField("observationId", GnirsImagingView.ObservationId, key = true, hidden = true),
      SqlField("camera", GnirsImagingView.Camera),
      SqlField("filters", GnirsImagingView.Filters),
    )

  private lazy val ConfigurationRequestGnirsImagingMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gnirsImaging")(
      SqlField("id", ConfigurationRequestView.GnirsImaging.Id, key = true, hidden = true),
      SqlField("camera", ConfigurationRequestView.GnirsImaging.Camera),
      SqlField("filters", ConfigurationRequestView.GnirsImaging.Filters),
    )
