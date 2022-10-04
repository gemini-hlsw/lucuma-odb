// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ProgramTable

trait CreateProgramResultMapping[F[_]] extends ProgramTable[F] {


  lazy val CreateProgramResultMapping =
    ObjectMapping(
      tpe = CreateProgramResultType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true),
        SqlObject("program"),
      )
    )

}

