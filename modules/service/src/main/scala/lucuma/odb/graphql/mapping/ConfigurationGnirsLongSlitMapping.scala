// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.GnirsLongSlitView

trait ConfigurationGnirsLongSlitMappings[F[_]]
  extends GnirsLongSlitView[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationGnirsLongSlitMappings = List(
    ConfigurationGnirsLongSlitMapping,
    ConfigurationRequestGnirsLongSlitMapping,
  )

  private lazy val ConfigurationGnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "gnirsLongSlit")(
      SqlField("observationId", GnirsLongSlitView.ObservationId, key = true, hidden = true),
      SqlField("camera", GnirsLongSlitView.Camera),
      SqlField("prism",  GnirsLongSlitView.PrismEffective),
    )

  private lazy val ConfigurationRequestGnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "gnirsLongSlit")(
      SqlField("id",     ConfigurationRequestView.GnirsLongSlit.Id, key = true, hidden = true),
      SqlField("camera", ConfigurationRequestView.GnirsLongSlit.Camera),
      SqlField("prism",  ConfigurationRequestView.GnirsLongSlit.Prism),
    )

}
