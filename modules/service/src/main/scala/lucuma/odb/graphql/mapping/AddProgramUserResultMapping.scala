// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping

import table.ProgramUserTable

trait AddProgramUserResultMapping[F[_]] extends ProgramUserTable[F]:

  lazy val AddProgramUserResultMapping: ObjectMapping =
    ObjectMapping(AddProgramUserResultType)(
      SqlField("id", ProgramUserTable.ProgramUserId, key = true, hidden = true),
      SqlObject("programUser")
    )