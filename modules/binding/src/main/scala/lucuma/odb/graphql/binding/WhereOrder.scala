// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package binding

import cats.Order
import cats.syntax.order.*
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

/**
 * The same criteria `binding` compiles into a Grackle predicate, kept as data so
 * they can be evaluated in memory instead.  Subscriptions filter events as they
 * arrive, with no query to push a predicate into, so a filter that wants ordering
 * ("at least RAPID") has to be applied here rather than in SQL.
 *
 * All supplied criteria must match, though usually only one is given.
 */
final case class WhereOrder[A: Order](
  eq:  Option[A],
  neq: Option[A],
  in:  Option[List[A]],
  nin: Option[List[A]],
  gt:  Option[A],
  lt:  Option[A],
  gte: Option[A],
  lte: Option[A]
):
  def matches(a: A): Boolean =
    eq.forall(_ === a)        &&
    neq.forall(_ =!= a)       &&
    in.forall(_.contains(a))  &&
    nin.forall(!_.contains(a)) &&
    gt.forall(a > _)          &&
    lt.forall(a < _)          &&
    gte.forall(a >= _)        &&
    lte.forall(a <= _)

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

  /** Like `binding`, but yields the criteria themselves for in-memory matching. */
  def inputBinding[A: Order](binding: Matcher[A]): Matcher[WhereOrder[A]] =
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
        (rEQ, rNEQ, rIN, rNIN, rGT, rLT, rGTE, rLTE).parMapN(WhereOrder.apply)
    }

}
