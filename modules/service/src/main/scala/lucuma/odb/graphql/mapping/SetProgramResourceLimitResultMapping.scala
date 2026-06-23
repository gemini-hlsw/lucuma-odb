// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProgramTable

trait SetProgramResourceLimitResultMapping[F[_]] extends ProgramTable[F] {

  lazy val SetProgramResourceLimitResultMapping =
    ObjectMapping(SetProgramResourceLimitResultType)(
      SqlField("id", ProgramTable.Id, key = true, hidden = true),
      SqlObject("program"),
    )

}
