// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView

trait ConfigurationTargetMapping[F[_]] extends ConfigurationRequestView[F] {

  lazy val ConfigurationTargetMappings =
    List(
      ConfigurationRequestConfigurationTargetMapping,
    )

  private lazy val ConfigurationRequestConfigurationTargetMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "target")(
      SqlField("synthetic-id", ConfigurationRequestView.Id, key = true, hidden = true),
      SqlObject("coordinates"),
      SqlObject("region"),
    )

}
