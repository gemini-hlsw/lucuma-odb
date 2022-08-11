// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.TargetView
import table.ProgramTable

import edu.gemini.grackle.skunk.SkunkMapping

trait TargetMapping[F[_]]
  extends ProgramTable[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val TargetType = schema.ref("Target")

  lazy val TargetMapping =
    ObjectMapping(
      tpe = TargetType,
      fieldMappings = List(
        SqlField("id", TargetView.TargetId, key = true),
        SqlField("existence", TargetView.Existence),
        SqlField("name", TargetView.Name),
        SqlObject("program", Join(TargetView.ProgramId, ProgramTable.Id)),
        SqlJson("sourceProfile", TargetView.SourceProfile),
        SqlObject("sidereal"),
        SqlObject("nonsidereal"),
      ),
    )

}

