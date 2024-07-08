// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.Order
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereOptionOrder {

  def binding[A: Order](path: Path, binding: Matcher[A]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        binding.Option("EQ", rEQ),
        binding.Option("NEQ", rNEQ),
        binding.List.Option("IN", rIN),
        binding.List.Option("NIN", rNIN),
        binding.Option("GT", rGT),
        binding.Option("LT", rLT),
        binding.Option("GTE", rGTE),
        binding.Option("LTE", rLTE)
      ) =>
        (rIsNull, rEQ, rNEQ, rIN, rNIN, rGT, rLT, rGTE, rLTE).parMapN {
          (isNull, EQ, NEQ, IN, NIN, GT, LT, GTE, LTE) =>
            and(List(
              isNull.map(IsNull(path, _)),
              EQ.map(a => Eql(path, Const(a.some))),
              NEQ.map(a => NEql(path, Const(a.some))),
              IN.map(as => In(path, as.map(_.some))),
              NIN.map(as => Not(In(path, as.map(_.some)))),
              GT.map(a => Gt(path, Const(a.some))),
              GTE.map(a => GtEql(path, Const(a.some))),
              LT.map(a => Lt(path, Const(a.some))),
              LTE.map(a => LtEql(path, Const(a.some)))
            ).flatten)
        }
    }

}
