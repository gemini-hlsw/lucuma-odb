// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.*

trait ProgramUserMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F] { this: SkunkMapping[F] =>

  lazy val ProgramUserType = schema.ref("ProgramUser")

  lazy val ProgramUserMapping =
    ObjectMapping(TypeMatch(ProgramUserType))(
      SqlField("programId", ProgramUserTable.ProgramId, hidden = true, key = true),
      SqlField("userId", ProgramUserTable.UserId, key = true),
      SqlField("role", ProgramUserTable.Role),
      SqlObject("user", Join(ProgramUserTable.UserId, UserTable.UserId))
    )

}

