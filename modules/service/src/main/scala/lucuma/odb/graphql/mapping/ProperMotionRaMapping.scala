// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.math.Angle

import table.TargetView
import table.ProgramTable

trait ProperMotionRaMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val ProperMotionRaMapping: ObjectMapping =
    ObjectMapping(ProperMotionRaType)(
      SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
      SqlField("value", TargetView.Sidereal.ProperMotion.Ra, hidden = true),
      FieldRef[Angle]("value").as("microarcsecondsPerYear", Angle.signedMicroarcseconds.get),
      FieldRef[Angle]("value").as("milliarcsecondsPerYear", Angle.signedDecimalMilliarcseconds.get),
    )

}

