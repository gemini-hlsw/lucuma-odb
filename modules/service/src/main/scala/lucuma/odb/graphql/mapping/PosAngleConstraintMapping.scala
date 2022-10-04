// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Angle
import lucuma.odb.graphql.util.MappingExtras

import table.ObservationView

trait PosAngleConstraintMapping[F[_]] extends ObservationView[F] {

  lazy val PosAngleConstraintMapping =
    ObjectMapping(
      tpe = PosAngleConstraintType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlField("mode",  ObservationView.PosAngleConstraint.Mode),
        SqlObject("angle")
      )
    )

}
