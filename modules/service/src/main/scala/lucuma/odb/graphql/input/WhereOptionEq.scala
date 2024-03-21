// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.Eq
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereOptionEq {

  def binding[A: Eq](path: Path, binding: Matcher[A]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        binding.Option("EQ", rEQ),
        binding.Option("NEQ", rNEQ),
        binding.List.Option("IN", rIN),
        binding.List.Option("NIN", rNIN)
      ) =>
        (rIsNull, rEQ, rNEQ, rIN, rNIN).parMapN {
          (isNull, EQ, NEQ, IN, NIN) =>
            and(List(
              isNull.map(IsNull(path, _)),
              EQ.map(a => Eql(path, Const(a))),
              NEQ.map(a => NEql(path, Const(a))),
              IN.map(as => In(path, as)),
              NIN.map(as => Not(In(path, as)))
            ).flatten)
        }
    }

}
