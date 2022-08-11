// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView

trait ConstraintSetMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val ConstraintSetType = schema.ref("ConstraintSet")

  lazy val ConstraintSetMapping =
    ObjectMapping(
      tpe = ConstraintSetType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlField("cloudExtinction", ObservationView.ConstraintSet.CloudExtinction),
        SqlField("imageQuality",    ObservationView.ConstraintSet.ImageQuality),
        SqlField("skyBackground",   ObservationView.ConstraintSet.SkyBackground),
        SqlField("waterVapor",      ObservationView.ConstraintSet.WaterVapor),
        SqlObject("elevationRange")
      )
    )

}

