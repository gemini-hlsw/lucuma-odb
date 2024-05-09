// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.*

trait UserMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F] { this: SkunkMapping[F] =>

  lazy val UserType = schema.ref("User")

  lazy val UserMapping =
    ObjectMapping(UserType)(
      SqlField("id", UserTable.UserId, key = true),
      SqlField("type", UserTable.UserType),
      SqlField("serviceName", UserTable.ServiceName),
      SqlField("orcidId", UserTable.OrcidId),
      SqlField("orcidGivenName", UserTable.OrcidGivenName),
      SqlField("orcidCreditName", UserTable.OrcidCreditName),
      SqlField("orcidFamilyName", UserTable.OrcidFamilyName),
      SqlField("orcidEmail", UserTable.OrcidEmail)
    )

}

