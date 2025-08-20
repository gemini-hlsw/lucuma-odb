// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ProgramUserView
import lucuma.odb.graphql.table.UserProfileTable
import lucuma.odb.graphql.table.UserTable

trait UserProfileMapping[F[_]] extends UserTable[F] with ProgramUserView[F]:

  private def profileMappingAt(
    path:    Path,
    profile: UserProfileTable[ColumnRef],
    idCol:   ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic-id", idCol, key = true, hidden = true),
      SqlField("givenName",  profile.GivenName),
      SqlField("familyName", profile.FamilyName),
      SqlField("creditName", profile.CreditName),
      SqlField("email",      profile.Email)
    )

  lazy val UserProfileMappings: List[TypeMapping] =
    List(
      profileMappingAt(UserType / "profile",  UserTable.Profile, UserTable.UserId),
      profileMappingAt(ProgramUserType / "preferredProfile", ProgramUserView.Preferred, ProgramUserView.UserId)
    )
