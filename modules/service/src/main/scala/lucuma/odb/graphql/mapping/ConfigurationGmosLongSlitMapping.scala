// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.GmosLongSlitView
import lucuma.odb.graphql.table.ConfigurationRequestTable

trait ConfigurationGmosLongSlitMappings[F[_]]
  extends GmosLongSlitView[F]
     with ConfigurationRequestTable[F] {

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
      SqlField("synthetic-id", ConfigurationRequestTable.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestTable.GmosNorthLongslitGrating),
    )

  // South

  lazy val ConfigurationGmosSouthLongSlitMappings = List(
    ConfigurationGmosSouthLongSlitMapping
  )
  
  private lazy val ConfigurationGmosSouthLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosSouthLongSlit")(
      SqlField("observationId", GmosSouthLongSlitView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthLongSlitView.Grating),
    )

}
