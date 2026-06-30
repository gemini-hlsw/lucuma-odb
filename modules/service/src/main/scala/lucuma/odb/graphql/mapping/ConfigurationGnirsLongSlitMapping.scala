// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GnirsSpectroscopyView

trait ConfigurationGnirsLongSlitMappings[F[_]]
  extends GnirsSpectroscopyView[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationGnirsLongSlitMappings = List(
    ConfigurationGnirsLongSlitMapping,
    ConfigurationRequestGnirsLongSlitMapping,
    ConfigurationGnirsIfuMapping,
    ConfigurationRequestGnirsIfuMapping,
  )

  private lazy val ConfigurationGnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gnirsLongSlit")(
      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),
      SqlField("grating", GnirsSpectroscopyView.GratingEffective),
      SqlField("camera",  GnirsSpectroscopyView.Camera),
      SqlField("prism",   GnirsSpectroscopyView.PrismEffective),
    )

  private lazy val ConfigurationRequestGnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gnirsLongSlit")(
      SqlField("id",      ConfigurationRequestView.GnirsLongSlit.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GnirsLongSlit.Grating),
      SqlField("camera",  ConfigurationRequestView.GnirsLongSlit.Camera),
      SqlField("prism",   ConfigurationRequestView.GnirsLongSlit.Prism),
    )

  private lazy val ConfigurationGnirsIfuMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gnirsIfu")(
      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),
      SqlField("grating", GnirsSpectroscopyView.GratingEffective),
      SqlField("fpu",     GnirsSpectroscopyView.FpuIfuConfig),
    )

  private lazy val ConfigurationRequestGnirsIfuMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gnirsIfu")(
      SqlField("id",      ConfigurationRequestView.GnirsIfu.Id, key = true, hidden = true),
      SqlField("grating", ConfigurationRequestView.GnirsIfu.Grating),
      SqlField("fpu",     ConfigurationRequestView.GnirsIfu.Fpu),
    )

}
