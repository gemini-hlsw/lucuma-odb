// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestTable
import lucuma.odb.graphql.table.ObservationView

trait ConfigurationConditionsMapping[F[_]]
  extends ObservationView[F] 
     with ConfigurationRequestTable[F] {

  lazy val ConfigurationConditionsMappings = List(
    ObservationConfigurationConditionsMapping,
    RequestConfigurationConditionsMapping
  )

  lazy val ObservationConfigurationConditionsMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "conditions")(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("cloudExtinction", ObservationView.ConstraintSet.CloudExtinction),
      SqlField("imageQuality",    ObservationView.ConstraintSet.ImageQuality),
      SqlField("skyBackground",   ObservationView.ConstraintSet.SkyBackground),
      SqlField("waterVapor",      ObservationView.ConstraintSet.WaterVapor),
    )

  lazy val RequestConfigurationConditionsMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "conditions")(
      SqlField("id", ConfigurationRequestTable.Id, key = true, hidden = true),
      SqlField("cloudExtinction", ConfigurationRequestTable.Conditions.CloudExtinction),
      SqlField("imageQuality",    ConfigurationRequestTable.Conditions.ImageQuality),
      SqlField("skyBackground",   ConfigurationRequestTable.Conditions.SkyBackground),
      SqlField("waterVapor",      ConfigurationRequestTable.Conditions.WaterVapor),
    )

}
