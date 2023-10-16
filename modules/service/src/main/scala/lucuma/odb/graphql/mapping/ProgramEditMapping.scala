// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.odb.data.EditType

import table.ProgramTable

trait ProgramEditMapping[F[_]] extends ProgramTable[F]  {

  // N.B. env is populated by the subscription elaborator
  lazy val ProgramEditMapping =
    ObjectMapping(
      tpe = ProgramEditType,
      fieldMappings = List(
        SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
        CursorField("id", _ => Result(0L), List("synthetic-id")),
        CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
        SqlObject("value")
      )
    )

}

