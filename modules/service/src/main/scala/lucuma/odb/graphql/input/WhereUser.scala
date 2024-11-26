// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.User
import lucuma.odb.data.UserType
import lucuma.odb.graphql.binding.*

object WhereUser:

  def binding(path: Path): Matcher[Predicate] =
    val WhereUserId          = WhereOrder.binding[User.Id](path / "id", UserIdBinding)
    val WhereType            = WhereEq.binding[UserType](path / "type", UserTypeBinding)
    val WhereOrcidId         = WhereOptionString.binding(path / "orcidId")
    val WherePrimaryProfile  = WhereUserProfile.binding(path / "primaryProfile")
    val WhereFallbackProfile = WhereUserProfile.binding(path / "fallbackProfile")

    lazy val WhereUserBinding = binding(path)
    ObjectFieldsBinding.rmap:
      case List(
        WhereUserBinding.List.Option("AND", rAND),
        WhereUserBinding.List.Option("OR", rOR),
        WhereUserBinding.Option("NOT", rNOT),
        WhereUserId.Option("id", rId),
        WhereType.Option("type", rType),
        WhereOrcidId.Option("orcidId", rOrcidId),
        WherePrimaryProfile.Option("primaryProfile", rPrimaryProfile),
        WhereFallbackProfile.Option("fallbackProfile", rFallbackProfile)
      ) =>
        (rAND, rOR, rNOT, rId, rType, rOrcidId, rPrimaryProfile, rFallbackProfile).parMapN:
          (AND, OR, NOT, id, t, orcid, primaryProfile, fallbackProfile) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              t,
              orcid,
              primaryProfile,
              fallbackProfile
            ).flatten)