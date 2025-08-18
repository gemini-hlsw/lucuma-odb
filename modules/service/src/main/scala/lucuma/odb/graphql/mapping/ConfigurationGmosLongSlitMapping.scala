// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GmosLongSlitTable

trait ConfigurationGmosLongSlitMappings[F[_]]
  extends GmosLongSlitTable[F]
     with ConfigurationRequestView[F] {

  // North

  lazy val ConfigurationGmosNorthLongSlitMappings = List(
    ConfigurationGmosNorthLongSlitMapping,
    ConfigurationRequestGmosNorthLongSlitMapping,
  )

  private lazy val ConfigurationGmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosNorthLongSlit")(
      SqlField("observationId", GmosNorthLongSlitTable.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosNorthLongSlitTable.Grating),
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
      SqlField("observationId", GmosSouthLongSlitTable.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthLongSlitTable.Grating),
    )

  private lazy val ConfigurationRequestGmosSouthLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosSouthLongSlit")(
      SqlField("id", ConfigurationRequestView.GmosSouthLongSlit.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosSouthLongSlit.Grating),
    )

}
