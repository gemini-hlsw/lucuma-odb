// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet.input

import cats.Order
import cats.syntax.parallel._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.model.{Observation, Program, Target}
import lucuma.odb.graphql.snippet.{ObservationIdBinding, ProgramIdBinding, TargetIdBinding}
import lucuma.odb.graphql.util.Bindings.{Matcher, ObjectFieldsBinding}

object WhereOrder {

  def Binding[A: Order](path: UniquePath[A], binding: Matcher[A]): Matcher[Predicate] =
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

  val ObservationId: Matcher[Predicate] =
    Binding(UniquePath[Observation.Id](List("id")), ObservationIdBinding)

  val ProgramId: Matcher[Predicate] =
    Binding(UniquePath[Program.Id](List("id")), ProgramIdBinding)

  val TargetId: Matcher[Predicate] =
    Binding(UniquePath[Target.Id](List("id")), TargetIdBinding)

}
