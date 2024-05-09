// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait ProperMotionMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val ProperMotionMapping =
    ObjectMapping(TypeMatch(ProperMotionType))(
      SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec"),
    )

}

