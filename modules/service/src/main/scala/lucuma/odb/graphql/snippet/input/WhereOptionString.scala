// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.all._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Term
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.graphql.util.Bindings._

object WhereOptionString {

  def binding(path: Term[Option[NonEmptyString]] with Path): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        NonEmptyStringBinding.Option("EQ", rEq),
        NonEmptyStringBinding.Option("NEQ", rNeq),
        NonEmptyStringBinding.List.Option("IN", rIn),
        NonEmptyStringBinding.List.Option("NIN", rNin),
        NonEmptyStringBinding.Option("LIKE", rLike),      // checked but unused, for now
        NonEmptyStringBinding.Option("NLIKE", rNlike),    // same
        BooleanBinding.Option("MATCH_CASE", rMatchCase)   // same
      ) =>
        (rIsNull, rEq, rNeq, rIn, rNin, rLike, rNlike, rMatchCase).mapN {
          (isNull, eq, neq, in, nin, _, _, _) =>
          and(List(
            isNull.map(IsNull(path, _)),
            eq.map(a => Eql(path, Const(a.some))),
            neq.map(a => NEql(path, Const(a.some))),
            in.map(as => In(path, as.map(_.some))),
            nin.map(as => Not(In(path, as.map(a => a.some)))),
          ).flatten)
        }
    }

}

