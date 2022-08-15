// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping

import table.ProgramEditTable
import table.ProgramTable

trait ProgramEditMapping[F[_]]
  extends ProgramEditTable[F]
     with ProgramTable[F] { this: SkunkMapping[F] =>

  lazy val ProgramEditType = schema.ref("ProgramEdit")

  lazy val ProgramEditMapping =
    ObjectMapping(
      tpe = ProgramEditType,
      fieldMappings = List(
        SqlField("id", ProgramEditTable.EventId, key = true),
        SqlField("editType", ProgramEditTable.EditType),
        SqlObject("value", Join(ProgramEditTable.ProgramId, ProgramTable.Id)),
      )
    )

}

