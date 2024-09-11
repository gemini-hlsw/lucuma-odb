// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.data.ObservingModeType

import table.*

trait ConfigurationObservingModeMappings[F[_]]
  extends ObservationView[F]
     with ConfigurationRequestView[F]
     with GmosLongSlitView[F] {

  lazy val ConfigurationObservingModeMappings = List(
    ConfigurationObservingModeMapping,
    ConfigurationRequestObservingModeMapping,
  )

  private lazy val ConfigurationObservingModeMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode")(
      SqlField("synthetic_id", ObservationView.ObservingMode.SyntheticId, key = true, hidden = true),
      FieldRef[ObservingModeType]("mode").as("instrument", _.instrument),
      SqlField("mode", ObservationView.ObservingMode.ObservingModeType),
      SqlObject("gmosNorthLongSlit", Join(ObservationView.Id, GmosNorthLongSlitView.Common.ObservationId)),
      SqlObject("gmosSouthLongSlit", Join(ObservationView.Id, GmosSouthLongSlitView.Common.ObservationId))
    )

  private lazy val ConfigurationRequestObservingModeMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode")(
      SqlField("synthetic_id", ConfigurationRequestView.Id, key = true, hidden = true),
      FieldRef[ObservingModeType]("mode").as("instrument", _.instrument),
      SqlField("mode", ConfigurationRequestView.ObservingModeType),      
      SqlObject("gmosNorthLongSlit"),
      SqlObject("gmosSouthLongSlit"),
    )
    
}
