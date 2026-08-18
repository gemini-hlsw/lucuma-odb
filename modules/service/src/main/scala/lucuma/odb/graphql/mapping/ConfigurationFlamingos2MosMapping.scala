// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.Flamingos2MosView

trait ConfigurationFlamingos2MosMappings[F[_]]
  extends Flamingos2MosView[F]
     with ConfigurationRequestView[F] {

  lazy val ConfigurationFlamingos2MosMappings = List(
    ConfigurationFlamingos2MosMapping,
    ConfigurationRequestFlamingos2MosMapping,
  )

  private lazy val ConfigurationFlamingos2MosMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "flamingos2Mos")(
      SqlField("observationId", Flamingos2MosView.ObservationId, key = true, hidden = true),
      SqlField("disperser", Flamingos2MosView.Disperser),
    )

  private lazy val ConfigurationRequestFlamingos2MosMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "flamingos2Mos")(
      SqlField("id", ConfigurationRequestView.Flamingos2Mos.Id, key = true, hidden = true),
      SqlField("disperser", ConfigurationRequestView.Flamingos2Mos.Disperser),
    )

}
