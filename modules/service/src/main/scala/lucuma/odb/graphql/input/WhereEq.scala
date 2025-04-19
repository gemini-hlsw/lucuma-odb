// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

class WhereEq[A: Eq](
  eq:  Option[A],
  neq: Option[A],
  in:  Option[List[A]],
  nin: Option[List[A]]
):
  def matches(a: A): Boolean =
    eq.forall(_ === a)       &&
    neq.forall(_ =!= a)      &&
    in.forall(_.contains(a)) &&
    nin.forall(!_.contains(a))

object WhereEq:

  def binding[A: Eq](path: Path, binding: Matcher[A]): Matcher[Predicate] =
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

  def inputBinding[A: Eq](binding: Matcher[A]): Matcher[WhereEq[A]] =
    ObjectFieldsBinding.rmap:
      case List(
        binding.Option("EQ", rEQ),
        binding.Option("NEQ", rNEQ),
        binding.List.Option("IN", rIN),
        binding.List.Option("NIN", rNIN)
      ) =>
        (rEQ, rNEQ, rIN, rNIN).parMapN((a,b,c,d) => WhereEq(a, b, c, d))