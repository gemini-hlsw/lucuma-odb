// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.ProgramUserTable

import edu.gemini.grackle.skunk.SkunkMapping

trait LinkUserResultMapping[F[_]]
  extends ProgramUserTable[F] { this: SkunkMapping[F] =>

  lazy val LinkUserResultType = schema.ref("LinkUserResult")

  lazy val LinkUserResultMapping =
    ObjectMapping(
      tpe = LinkUserResultType,
      fieldMappings = List(
        SqlField("programId", ProgramUserTable.ProgramId, hidden = true, key = true),
        SqlField("userId", ProgramUserTable.UserId, key = true),
        SqlObject("user"),
      )
    )

}

