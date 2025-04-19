// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ObservationView

trait ConstraintSetMapping[F[_]] extends ObservationView[F] {

  lazy val ConstraintSetMapping =
    ObjectMapping(ConstraintSetType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("cloudExtinction", ObservationView.ConstraintSet.CloudExtinction),
      SqlField("imageQuality",    ObservationView.ConstraintSet.ImageQuality),
      SqlField("skyBackground",   ObservationView.ConstraintSet.SkyBackground),
      SqlField("waterVapor",      ObservationView.ConstraintSet.WaterVapor),
      SqlObject("elevationRange")
    )

}

