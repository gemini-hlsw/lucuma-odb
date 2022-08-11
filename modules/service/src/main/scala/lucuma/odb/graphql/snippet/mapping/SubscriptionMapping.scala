// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import fs2.Stream
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbMapping.Topics
import lucuma.odb.graphql.snippet.input.ProgramEditInput
import lucuma.odb.graphql.snippet.predicates.ProgramPredicates
import lucuma.odb.graphql.util.Bindings.Matcher
import lucuma.odb.graphql.util.MutationCompanionOps
import lucuma.odb.instances.given
import org.tpolecat.typename.TypeName
import edu.gemini.grackle.Predicate._

import scala.reflect.ClassTag
import edu.gemini.grackle.Path.UniquePath

trait SubscriptionMapping[F[_]]
  extends MutationCompanionOps[F]
     with ProgramPredicates[F]
  { this: SkunkMapping[F] =>

  def topics: Topics[F]
  def user: User

  lazy val SubscriptionType = schema.ref("Subscription")

  private lazy val subscriptionFields: List[SubscriptionField] =
    List(
      ProgramEdit,
    )

  lazy val SubscriptionMapping =
    ObjectMapping(tpe = SubscriptionType, fieldMappings = subscriptionFields.map(_.FieldMapping))

  lazy val SubscriptionElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    subscriptionFields.foldMap(mf => Map(SubscriptionType -> mf.Elaborator))

  // Convenience for constructing a Subscription stream and corresponding 1-arg elaborator.
  private trait SubscriptionField {
    def Elaborator: PartialFunction[Select, Result[Query]]
    def FieldMapping: SqlRoot
  }
  private object SubscriptionField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => Stream[F, Result[Query]]) =
      new SubscriptionField {
        val FieldMapping = SqlRoot(fieldName, mutation = Mutation.simpleStream((child, env) => env.getR[I]("input").flatTraverse(f(_, child))))
        val Elaborator =
          case Select(`fieldName`, List(inputBinding("input", rInput)), child) =>
            rInput.map(input => Environment(Env("input" -> input), Select(fieldName, Nil, child)))
      }
  }

  private val ProgramEdit =
    SubscriptionField("programEdit", ProgramEditInput.Binding.Option) { (input, child) =>
      topics
        .program
        .subscribe(1024)
        .filter(e => e.canRead(user) && input.flatMap(_.programId).forall(_ === e.programId))
        .map(e => Result(Unique(Filter(Eql(UniquePath(List("value", "id")), Const(e.programId)), child))))
    }

}

