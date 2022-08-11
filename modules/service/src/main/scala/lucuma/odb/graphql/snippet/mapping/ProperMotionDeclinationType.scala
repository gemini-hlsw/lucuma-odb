// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Angle
import lucuma.odb.graphql.util.MappingExtras

import table.TargetView
import table.ProgramTable

trait ProperMotionDeclinationMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F]
     with MappingExtras[F] { this: SkunkMapping[F] =>

  lazy val ProperMotionDeclinationType = schema.ref("ProperMotionDeclination")

  lazy val ProperMotionDeclinationMapping =
    ObjectMapping(
      tpe = ProperMotionDeclinationType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
        SqlField("value", TargetView.Sidereal.ProperMotion.Dec, hidden = true),
        FieldRef[Angle]("value").as("microarcsecondsPerYear", Angle.signedMicroarcseconds.get),
        FieldRef[Angle]("value").as("milliarcsecondsPerYear", Angle.signedDecimalMilliarcseconds.get),
      )
    )

  }

