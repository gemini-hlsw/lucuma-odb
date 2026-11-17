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

/** Matches an `Attribution`: who took an action and when. */
object WhereAttribution:

  def binding(path: Path): Matcher[Predicate] =
    val ByBinding = WhereOrder.binding[User.Id](path / "by", UserIdBinding)
    val AtBinding = WhereOrder.binding[Timestamp](path / "at", TimestampBinding)
    ObjectFieldsBinding.rmap:
      case List(
        ByBinding.Option("by", rBy),
        AtBinding.Option("at", rAt)
      ) =>
        (rBy, rAt).parMapN((by, at) => and(List(by, at).flatten))

  /**
   * An optional attribution, with `IS_NULL` matched by `isNull`.  When the
   * attribution belongs to one of a list of rows, `isNull` should compare a
   * column that is never null, rather than test for null: a parent without
   * rows has nulls in their place, which an IS NULL test would match.
   */
  def optionBinding(path: Path, isNull: Boolean => Predicate): Matcher[Predicate] =
    val ByBinding = WhereOrder.binding[User.Id](path / "by", UserIdBinding)
    val AtBinding = WhereOrder.binding[Timestamp](path / "at", TimestampBinding)
    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        ByBinding.Option("by", rBy),
        AtBinding.Option("at", rAt)
      ) =>
        (rIsNull, rBy, rAt).parMapN: (n, by, at) =>
          and(List(n.map(isNull), by, at).flatten)
