// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait SiderealMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val SiderealMapping =
    ObjectMapping(SiderealType)(
      SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
      SqlObject("ra"),
      SqlObject("dec"),
      SqlField("epoch", TargetView.Sidereal.Epoch),
      SqlObject("properMotion"),
      SqlObject("radialVelocity"),
      SqlObject("parallax"),
      SqlObject("catalogInfo"),
    )

  }

