// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GmosMosView

trait ConfigurationGmosMosMappings[F[_]]
  extends GmosMosView[F]
     with ConfigurationRequestView[F] {

  // North

  lazy val ConfigurationGmosNorthMosMappings = List(
    ConfigurationGmosNorthMosMapping,
    ConfigurationRequestGmosNorthMosMapping,
  )

  private lazy val ConfigurationGmosNorthMosMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosNorthMos")(
      SqlField("observationId", GmosNorthMosView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosNorthMosView.Grating),
    )

  private lazy val ConfigurationRequestGmosNorthMosMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosNorthMos")(
      SqlField("id", ConfigurationRequestView.GmosNorthMos.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosNorthMos.Grating),
    )

  // South

  lazy val ConfigurationGmosSouthMosMappings = List(
    ConfigurationGmosSouthMosMapping,
    ConfigurationRequestGmosSouthMosMapping,
  )

  private lazy val ConfigurationGmosSouthMosMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosSouthMos")(
      SqlField("observationId", GmosSouthMosView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthMosView.Grating),
    )

  private lazy val ConfigurationRequestGmosSouthMosMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosSouthMos")(
      SqlField("id", ConfigurationRequestView.GmosSouthMos.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosSouthMos.Grating),
    )

}
