// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GmosLongSlitView

trait ConfigurationGmosLongSlitMappings[F[_]]
  extends GmosLongSlitView[F]
     with ConfigurationRequestView[F] {

  // North

  lazy val ConfigurationGmosNorthLongSlitMappings = List(
    ConfigurationGmosNorthLongSlitMapping,
    ConfigurationRequestGmosNorthLongSlitMapping,
  )

  private lazy val ConfigurationGmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosNorthLongSlit")(
      SqlField("observationId", GmosNorthLongSlitView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosNorthLongSlitView.Grating),
    )

  private lazy val ConfigurationRequestGmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosNorthLongSlit")(
      SqlField("id", ConfigurationRequestView.GmosNorthLongSlit.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosNorthLongSlit.Grating),
    )

  // South

  lazy val ConfigurationGmosSouthLongSlitMappings = List(
    ConfigurationGmosSouthLongSlitMapping,
    ConfigurationRequestGmosSouthLongSlitMapping,
  )

  private lazy val ConfigurationGmosSouthLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosSouthLongSlit")(
      SqlField("observationId", GmosSouthLongSlitView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthLongSlitView.Grating),
    )

  private lazy val ConfigurationRequestGmosSouthLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosSouthLongSlit")(
      SqlField("id", ConfigurationRequestView.GmosSouthLongSlit.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosSouthLongSlit.Grating),
    )

}
