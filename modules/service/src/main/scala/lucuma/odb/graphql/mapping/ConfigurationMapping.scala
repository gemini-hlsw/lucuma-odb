// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ObservationView

trait ConfigurationMapping[F[_]]
  extends ObservationView[F] with ConfigurationRequestView[F] {

  lazy val ConfigurationMappings =
    List(
      ObservationConfigurationMapping,
      ConfigurationRequestConfigurationMapping,
    )

  private lazy val ConfigurationRequestConfigurationMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration")(
      SqlField("synthetic-id", ConfigurationRequestView.Id, key = true, hidden = true),
      SqlObject("target"),
      SqlObject("conditions"),
      SqlObject("observingMode"),
    )

  private lazy val ObservationConfigurationMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration")(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      SqlObject("target"),
      SqlObject("conditions"),
      SqlObject("observingMode"),
    )

}
