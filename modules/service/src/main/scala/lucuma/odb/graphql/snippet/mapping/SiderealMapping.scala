// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.TargetView
import table.ProgramTable

import edu.gemini.grackle.skunk.SkunkMapping

trait SiderealMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val SiderealType = schema.ref("Sidereal")

  lazy val SiderealMapping =
    ObjectMapping(
      tpe = SiderealType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
        SqlObject("ra"),
        SqlObject("dec"),
        SqlField("epoch", TargetView.Sidereal.Epoch),
        SqlObject("properMotion"),
        SqlObject("radialVelocity"),
        SqlObject("parallax"),
        SqlObject("catalogInfo"),
      ),
    )

  }

