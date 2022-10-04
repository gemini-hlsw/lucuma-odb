// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Angle
import lucuma.odb.graphql.util.MappingExtras

import table.TargetView
import table.ProgramTable

trait ProperMotionRAMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val ProperMotionRAMapping =
    ObjectMapping(
      tpe = ProperMotionRAType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
        SqlField("value", TargetView.Sidereal.ProperMotion.Ra, hidden = true),
        FieldRef[Angle]("value").as("microarcsecondsPerYear", Angle.signedMicroarcseconds.get),
        FieldRef[Angle]("value").as("milliarcsecondsPerYear", Angle.signedDecimalMilliarcseconds.get),
      )
    )

  }

