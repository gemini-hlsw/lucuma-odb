package lucuma.odb.graphql
package snippet
package input

import lucuma.odb.graphql.util.Bindings._
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import cats.implicits._
import edu.gemini.grackle.Path.UniquePath
import lucuma.core.model.Program

object WhereOrderProgramId {

  private val Id = UniquePath[Program.Id](List("id"))

  val Binding: Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("EQ", rEQ),
        ProgramIdBinding.Option("NEQ", rNEQ),
        ProgramIdBinding.List.Option("IN", rIN),
        ProgramIdBinding.List.Option("NIN", rNIN),
        ProgramIdBinding.Option("GT", rGT),
        ProgramIdBinding.Option("LT", rLT),
        ProgramIdBinding.Option("GTE", rGTE),
        ProgramIdBinding.Option("LTE", rLTE),
      ) =>
      (rEQ, rNEQ, rIN, rNIN, rGT, rLT, rGTE, rLTE).parMapN {
        (EQ, NEQ, IN, NIN, GT, LT, GTE, LTE) =>
          and(List(
            EQ.map(a => Eql(Id, Const(a))),
            NEQ.map(a => NEql(Id, Const(a))),
            IN.map(as => In(Id, as)),
            NIN.map(as => Not(In(Id, as))),
            GT.map(a => Gt(Id, Const(a))),
            GTE.map(a => GtEql(Id, Const(a))),
            LT.map(a => Lt(Id, Const(a))),
            LTE.map(a => LtEql(Id, Const(a))),
          ).flatten)
      }
    }

}