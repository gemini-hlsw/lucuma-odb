// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Group
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.odb.graphql.binding.*

/**
 * Matches a `GroupConstraintWaiver`, field by field.  Every condition compares
 * a column that a waiver always has, never testing for null: a group without
 * waivers has nulls in their place, and so matches no condition, negated or
 * not.  A revocation's presence is matched through the waiver's `active`.
 */
object WhereGroupConstraintWaiver:

  def binding(path: Path): Matcher[Predicate] =
    val IdBinding        = WhereOrder.binding[GroupConstraintWaiver.Id](path / "id", GroupConstraintWaiverIdBinding)
    val GroupIdBinding_  = WhereOrder.binding[Group.Id](path / "groupId", GroupIdBinding)
    val ViolationBinding = WhereGroupConstraintViolation.binding(path / "violation")
    val GrantedBinding   = WhereAttribution.binding(path / "granted")
    val RevokedBinding   = WhereAttribution.optionBinding(path / "revoked", isNull => Eql(path / "active", Const(isNull)))
    lazy val WhereWaiverBinding = binding(path)
    ObjectFieldsBinding.rmap:
      case List(
        WhereWaiverBinding.List.Option("AND", rAND),
        WhereWaiverBinding.List.Option("OR", rOR),
        WhereWaiverBinding.Option("NOT", rNOT),
        IdBinding.Option("id", rId),
        GroupIdBinding_.Option("groupId", rGroupId),
        ViolationBinding.Option("violation", rViolation),
        GrantedBinding.Option("granted", rGranted),
        RevokedBinding.Option("revoked", rRevoked)
      ) =>
        (rAND, rOR, rNOT, rId, rGroupId, rViolation, rGranted, rRevoked).parMapN:
          (all, any, not, id, groupId, violation, granted, revoked) =>
            and(List(
              all.map(and),
              any.map(or),
              not.map(Not(_)),
              id,
              groupId,
              violation,
              granted,
              revoked
            ).flatten)
