// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

object WhereGroupConstraintWaiver:

  def binding(path: Path): Matcher[Predicate] =
    val CodeBinding      = WhereEq.binding(path / "violation" / "code", GroupConstraintViolationCodeBinding)
    val ActiveBinding    = WhereBoolean.binding(path / "active", BooleanBinding)
    val GrantedByBinding = WhereOrder.binding[User.Id](path / "granted" / "by", UserIdBinding)
    val GrantedAtBinding = WhereOrder.binding[Timestamp](path / "granted" / "at", TimestampBinding)
    lazy val WhereWaiverBinding = binding(path)
    ObjectFieldsBinding.rmap:
      case List(
        WhereWaiverBinding.List.Option("AND", rAND),
        WhereWaiverBinding.List.Option("OR", rOR),
        WhereWaiverBinding.Option("NOT", rNOT),
        CodeBinding.Option("code", rCode),
        ActiveBinding.Option("active", rActive),
        GrantedByBinding.Option("grantedBy", rGrantedBy),
        GrantedAtBinding.Option("grantedAt", rGrantedAt)
      ) =>
        (rAND, rOR, rNOT, rCode, rActive, rGrantedBy, rGrantedAt).parMapN: (AND, OR, NOT, code, active, grantedBy, grantedAt) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            code,
            active,
            grantedBy,
            grantedAt
          ).flatten)
