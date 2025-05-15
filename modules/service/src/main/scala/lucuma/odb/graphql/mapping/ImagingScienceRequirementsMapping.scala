// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView


trait ImagingScienceRequirementsMapping[F[_]] extends ObservationView[F] {

  import ObservationView.ScienceRequirements.Imaging

  lazy val ImagingScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ImagingScienceRequirementsType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("exposureTimeMode"),
      SqlObject("minimumFov"),
      SqlField("narrowFilters", Imaging.NarrowFilters),
      SqlField("broadFilters", Imaging.BroadFilters),
      SqlObject("gmosNorth", Join(List(
        ObservationView.Id -> Imaging.ImagingGmosNorthTable.Id,
      ))),
      SqlObject("gmosSouth", Join(List(
        ObservationView.Id -> Imaging.ImagingGmosSouthTable.Id,
      ))),
    )

  lazy val ImagingGmosNorthScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ImagingGmosNorthScienceRequirementsType)(
      SqlField("id",      Imaging.ImagingGmosNorthTable.Id, key = true, hidden = true),
      SqlField("filters", Imaging.ImagingGmosNorthTable.Filter),
    )

  lazy val ImagingGmosSouthScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ImagingGmosSouthScienceRequirementsType)(
      SqlField("id",      Imaging.ImagingGmosSouthTable.Id, key = true, hidden = true),
      SqlField("filters", Imaging.ImagingGmosSouthTable.Filter),
    )
}
