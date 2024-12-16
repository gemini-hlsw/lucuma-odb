// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.graphql.binding.*

case class UserProfileInput(
  givenName:  Nullable[String],
  familyName: Nullable[String],
  creditName: Nullable[String],
  email:      Nullable[String]
)

object UserProfileInput:
  val Empty: UserProfileInput =
    UserProfileInput(Absent, Absent, Absent, Absent)

  val Binding: Matcher[UserProfileInput] =
    ObjectFieldsBinding.rmap:
      case List(
        StringBinding.Nullable("givenName",  rGivenName),
        StringBinding.Nullable("familyName", rFamilyName),
        StringBinding.Nullable("creditName", rCreditName),
        StringBinding.Nullable("email",      rEmail)
      ) => (rGivenName, rFamilyName, rCreditName, rEmail).parMapN(UserProfileInput.apply)