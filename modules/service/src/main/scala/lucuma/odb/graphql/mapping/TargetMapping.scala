// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable

trait TargetMapping[F[_]] extends ProgramTable[F] with TargetView[F] {

  lazy val TargetMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetType,
      fieldMappings = List(
        SqlField("id", TargetView.TargetId, key = true),
        SqlField("existence", TargetView.Existence),
        SqlField("name", TargetView.Name),
        SqlObject("program", Join(TargetView.ProgramId, ProgramTable.Id)),
        SqlJson("sourceProfile", TargetView.SourceProfile),
        SqlField("role", TargetView.Role, hidden = true),
        SqlObject("sidereal"),
        SqlObject("nonsidereal")
      )
    )

}

