// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.Flamingos2ImagingView

trait ConfigurationFlamingos2ImagingMappings[F[_]]
  extends Flamingos2ImagingView[F]
     with ConfigurationRequestView[F]:

  lazy val ConfigurationFlamingos2ImagingMappings = List(
    ConfigurationFlamingos2ImagingMapping,
    ConfigurationRequestFlamingos2ImagingMapping,
  )

  private lazy val ConfigurationFlamingos2ImagingMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration" / "observingMode" / "flamingos2Imaging")(
      SqlField("observationId", Flamingos2ImagingView.ObservationId, key = true, hidden = true),
      SqlField("filters", Flamingos2ImagingView.Filters),
    )

  private lazy val ConfigurationRequestFlamingos2ImagingMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration" / "observingMode" / "flamingos2Imaging")(
      SqlField("id", ConfigurationRequestView.Flamingos2Imaging.Id, key = true, hidden = true),
      SqlField("filters", ConfigurationRequestView.Flamingos2Imaging.Filters),
    )
