// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.cats.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Term
import grackle.sql.Like
import lucuma.odb.graphql.binding.*

object WhereString {

  def binding(path: Path): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("EQ", rEq),
        NonEmptyStringBinding.Option("NEQ", rNeq),
        NonEmptyStringBinding.List.Option("IN", rIn),
        NonEmptyStringBinding.List.Option("NIN", rNin),
        NonEmptyStringBinding.Option("LIKE", rLike),
        NonEmptyStringBinding.Option("NLIKE", rNlike),
        BooleanBinding.Option("MATCH_CASE", rMatchCase)
      ) =>
        (rEq, rNeq, rIn, rNin, rLike, rNlike, rMatchCase).mapN {
          (eq, neq, in, nin, like, nlike, matchCase) =>
          and(List(
            eq.map(a => Eql(path, Const(a))),
            neq.map(a => NEql(path, Const(a))),
            in.map(as => In(path, as)),
            nin.map(as => Not(In(path, as.map(a => a)))),
            like.map(s => Like(path, s.value, !matchCase.getOrElse(true))),
            nlike.map(s => Not(Like(path, s.value, !matchCase.getOrElse(true)))),
          ).flatten)
        }
    }

}

