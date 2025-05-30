// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView

trait ImagingScienceRequirementsMapping[F[_]] extends ObservationView[F]:

  import ObservationView.ScienceRequirements.Imaging

  lazy val ImagingScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ImagingScienceRequirementsType)(
      SqlField("id", Imaging.SyntheticId, key = true, hidden = true),
      SqlObject("minimumFov"),
      SqlField("narrowFilters", Imaging.NarrowFilters),
      SqlField("broadFilters", Imaging.BroadFilters),
      SqlField("combinedFilters", Imaging.CombinedFilters),
    )
