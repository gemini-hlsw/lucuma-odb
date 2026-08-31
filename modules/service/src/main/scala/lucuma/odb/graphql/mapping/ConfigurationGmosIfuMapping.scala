// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GmosIfuView

/**
 * The GMOS IFU configuration: grating and aperture.  Unlike GNIRS the mode has its own view, so
 * the join alone discriminates and no extra key column is needed.
 */
trait ConfigurationGmosIfuMappings[F[_]]
  extends GmosIfuView[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationGmosIfuMappings = List(
    ConfigurationGmosNorthIfuMapping,
    ConfigurationRequestGmosNorthIfuMapping,
    ConfigurationGmosSouthIfuMapping,
    ConfigurationRequestGmosSouthIfuMapping,
  )

  private lazy val ConfigurationGmosNorthIfuMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosNorthIfu")(
      SqlField("observationId", GmosNorthIfuView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosNorthIfuView.Grating),
      SqlField("fpu",     GmosNorthIfuView.Fpu),
    )

  private lazy val ConfigurationRequestGmosNorthIfuMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosNorthIfu")(
      SqlField("id",      ConfigurationRequestView.GmosNorthIfu.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosNorthIfu.Grating),
      SqlField("fpu",     ConfigurationRequestView.GmosNorthIfu.Fpu),
    )

  private lazy val ConfigurationGmosSouthIfuMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gmosSouthIfu")(
      SqlField("observationId", GmosSouthIfuView.Common.ObservationId, key = true, hidden = true),
      SqlField("grating", GmosSouthIfuView.Grating),
      SqlField("fpu",     GmosSouthIfuView.Fpu),
    )

  private lazy val ConfigurationRequestGmosSouthIfuMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gmosSouthIfu")(
      SqlField("id",      ConfigurationRequestView.GmosSouthIfu.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GmosSouthIfu.Grating),
      SqlField("fpu",     ConfigurationRequestView.GmosSouthIfu.Fpu),
    )

}
