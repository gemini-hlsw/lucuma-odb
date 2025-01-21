// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping

import table.ProgramUserTable

trait ChangeProgramUserRoleResultMapping[F[_]] extends ProgramUserTable[F]:

  lazy val ChangeProgramUserRoleResultMapping: ObjectMapping =
    ObjectMapping(ChangeProgramUserRoleResultType)(
      SqlField("id", ProgramUserTable.ProgramUserId, key = true, hidden = true),
      SqlObject("programUser")
    )