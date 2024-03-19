// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Order
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

/** For EnumTypes where ordering doesn't make sense */
object WhereUnorderedTag {

  def binding[A: Order](path: Path, binding: Matcher[A]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        binding.Option("EQ", rEQ),
        binding.Option("NEQ", rNEQ),
        binding.List.Option("IN", rIN),
        binding.List.Option("NIN", rNIN)
      ) =>
        (rEQ, rNEQ, rIN, rNIN).parMapN {
          (EQ, NEQ, IN, NIN) =>
            and(List(
              EQ.map(a => Eql(path, Const(a))),
              NEQ.map(a => NEql(path, Const(a))),
              IN.map(as => In(path, as)),
              NIN.map(as => Not(In(path, as)))
            ).flatten)
        }
    }
}
