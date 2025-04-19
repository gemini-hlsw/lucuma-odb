// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Order
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereOrder {

  def binding[A: Order](path: Path, binding: Matcher[A]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        binding.Option("EQ", rEQ),
        binding.Option("NEQ", rNEQ),
        binding.List.Option("IN", rIN),
        binding.List.Option("NIN", rNIN),
        binding.Option("GT", rGT),
        binding.Option("LT", rLT),
        binding.Option("GTE", rGTE),
        binding.Option("LTE", rLTE)
      ) =>
        (rEQ, rNEQ, rIN, rNIN, rGT, rLT, rGTE, rLTE).parMapN {
          (EQ, NEQ, IN, NIN, GT, LT, GTE, LTE) =>
            and(List(
              EQ.map(a => Eql(path, Const(a))),
              NEQ.map(a => NEql(path, Const(a))),
              IN.map(as => In(path, as)),
              NIN.map(as => Not(In(path, as))),
              GT.map(a => Gt(path, Const(a))),
              GTE.map(a => GtEql(path, Const(a))),
              LT.map(a => Lt(path, Const(a))),
              LTE.map(a => LtEql(path, Const(a)))
            ).flatten)
        }
    }

}
