// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package binding

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Term
import grackle.sql.Like
import lucuma.odb.graphql.binding.*

object WhereOptionString {

  def binding(path: Path): Matcher[Predicate] =
    bindingAs(path, NonEmptyStringBinding)

  /**
   * As `binding`, but parses the equality-style values (EQ, NEQ, IN, NIN) with the
   * given matcher. Use this when the mapped column's Scala type is not `String`,
   * since the constants are bound into the query with the column's own codec.
   * LIKE and NLIKE remain string comparisons, which is always correct because
   * Grackle encodes them with a string encoder regardless of the column codec.
   */
  def bindingAs[A: Eq](path: Path, value: Matcher[A]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        value.Option("EQ", rEq),
        value.Option("NEQ", rNeq),
        value.List.Option("IN", rIn),
        value.List.Option("NIN", rNin),
        NonEmptyStringBinding.Option("LIKE", rLike),
        NonEmptyStringBinding.Option("NLIKE", rNlike),
        BooleanBinding.Option("MATCH_CASE", rMatchCase)
      ) =>
        (rIsNull, rEq, rNeq, rIn, rNin, rLike, rNlike, rMatchCase).mapN {
          (isNull, eq, neq, in, nin, like, nlike, matchCase) =>
          and(List(
            isNull.map(IsNull(path, _)),
            eq.map(a => Eql(path, Const(a.some))),
            neq.map(a => NEql(path, Const(a.some))),
            in.map(as => In(path, as.map(_.some))),
            nin.map(as => Not(In(path, as.map(a => a.some)))),
            // the casts below are safe; the type parameter is a phantom in this case
            like.map(s => Like(path, s.value, !matchCase.getOrElse(true))),
            nlike.map(s => Not(Like(path, s.value, !matchCase.getOrElse(true)))),
          ).flatten)
        }
    }

}

