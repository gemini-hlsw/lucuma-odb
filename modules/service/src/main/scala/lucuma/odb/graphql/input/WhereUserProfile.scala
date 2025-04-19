// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereUserProfile:

  def binding(path: Path): Matcher[Predicate] =
    val WhereGivenName  = WhereOptionString.binding(path / "givenName")
    val WhereCreditName = WhereOptionString.binding(path / "creditName")
    val WhereFamilyName = WhereOptionString.binding(path / "familyName")
    val WhereEmail      = WhereOptionString.binding(path / "email")

    ObjectFieldsBinding.rmap:
      case List(
        WhereGivenName.Option("givenName", rGivenName),
        WhereCreditName.Option("creditName", rCreditName),
        WhereFamilyName.Option("familyName", rFamilyName),
        WhereEmail.Option("email", rEmail)
      ) =>
        (rGivenName, rCreditName, rFamilyName, rEmail).parMapN:
          (givenName, creditName, familyName, email) =>
            and(List(givenName, creditName, familyName, email).flatten)