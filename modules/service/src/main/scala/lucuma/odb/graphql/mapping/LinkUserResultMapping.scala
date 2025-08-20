// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProgramUserView

trait LinkUserResultMapping[F[_]] extends ProgramUserView[F]  {

  lazy val LinkUserResultMapping =
    ObjectMapping(LinkUserResultType)(
      SqlField("id", ProgramUserView.ProgramUserId, hidden = true, key = true),
      SqlObject("user")
    )

}

