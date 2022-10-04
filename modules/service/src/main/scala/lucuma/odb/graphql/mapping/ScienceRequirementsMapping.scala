// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView

trait ScienceRequirementsMapping[F[_]] extends ObservationView[F] {

  import ObservationView.ScienceRequirements

  lazy val ScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(
      tpe = ScienceRequirementsType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlField("mode", ScienceRequirements.Mode),
        SqlObject("spectroscopy")
      )
    )
}
