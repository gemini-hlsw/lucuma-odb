// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.Flamingos2LongSlitView

trait ConfigurationFlamingos2LongSlitMappings[F[_]]
  extends Flamingos2LongSlitView[F]
     with ConfigurationRequestView[F] {

  // North

  lazy val ConfigurationFlamingos2LongSlitMappings = List(
    ConfigurationFlamingos2LongSlitMapping,
    ConfigurationRequestFlamingos2LongSlitMapping,
  )

  private lazy val ConfigurationFlamingos2LongSlitMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "flamingos2LongSlit")(
      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),
      SqlField("disperser", Flamingos2LongSlitView.Disperser),
    )

  private lazy val ConfigurationRequestFlamingos2LongSlitMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "flamingos2LongSlit")(
      SqlField("id", ConfigurationRequestView.Flamingos2LongSlit.Id, key = true, hidden = true),
      SqlField("disperser", ConfigurationRequestView.Flamingos2LongSlit.Disperser),
    )

}
