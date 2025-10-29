// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GmosImagingView

trait ConfigurationGmosImagingMappings[F[_]] extends GmosImagingView[F]
                                                with ConfigurationRequestView[F]:

  // North

  lazy val ConfigurationGmosNorthImagingMappings = List(
    ConfigurationGmosNorthImagingMapping,
    ConfigurationRequestGmosNorthImagingMapping,
  )

  private lazy val ConfigurationGmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosNorthImaging")(
      SqlField("observationId", GmosNorthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosNorthImagingView.Filters),
    )

  private lazy val ConfigurationRequestGmosNorthImagingMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosNorthImaging")(
      SqlField("id", ConfigurationRequestView.GmosNorthImaging.Id, key = true, hidden = true),
      SqlField("filters", ConfigurationRequestView.GmosNorthImaging.Filters),
    )

  // South

  lazy val ConfigurationGmosSouthImagingMappings = List(
    ConfigurationGmosSouthImagingMapping,
    ConfigurationRequestGmosSouthImagingMapping,
  )
  
  private lazy val ConfigurationGmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosSouthImaging")(
      SqlField("observationId", GmosSouthImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", GmosSouthImagingView.Filters),
    )

  private lazy val ConfigurationRequestGmosSouthImagingMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosSouthImaging")(
      SqlField("id", ConfigurationRequestView.GmosSouthImaging.Id, key = true, hidden = true),
      SqlField("filters", ConfigurationRequestView.GmosSouthImaging.Filters),
    )