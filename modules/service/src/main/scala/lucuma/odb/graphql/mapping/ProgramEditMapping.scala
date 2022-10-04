// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Result
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.data.EditType

import table.ProgramEditTable
import table.ProgramTable

trait ProgramEditMapping[F[_]] extends ProgramTable[F]  {

  // N.B. env is populated by the subscription elaborator
  lazy val ProgramEditMapping =
    ObjectMapping(
      tpe = ProgramEditType,
      fieldMappings = List(
        SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
        CursorField("id", _.envR[Long]("id")),
        CursorField("editType", _.envR[EditType]("editType")),
        SqlObject("value")
      )
    )

}

