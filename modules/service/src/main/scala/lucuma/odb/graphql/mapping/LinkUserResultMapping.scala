// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProgramUserTable

trait LinkUserResultMapping[F[_]] extends ProgramUserTable[F]  {

  lazy val LinkUserResultMapping =
    ObjectMapping(LinkUserResultType)(
      SqlField("programId", ProgramUserTable.ProgramId, hidden = true, key = true),
      SqlField("userId", ProgramUserTable.UserId, key = true, hidden = true),
      SqlObject("user"),
    )

}

