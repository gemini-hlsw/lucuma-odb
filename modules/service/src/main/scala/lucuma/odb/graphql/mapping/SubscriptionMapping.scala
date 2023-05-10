// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.data.Nested
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import fs2.Stream
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbMapping.Topics
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.input.GroupEditInput
import lucuma.odb.graphql.input.ObservationEditInput
import lucuma.odb.graphql.input.ProgramEditInput
import lucuma.odb.graphql.input.TargetEditInput
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import org.tpolecat.typename.TypeName

import scala.reflect.ClassTag

trait SubscriptionMapping[F[_]] extends Predicates[F] {

  def topics: Topics[F]
  def user: User

  lazy val SubscriptionType = schema.ref("Subscription")

  private lazy val subscriptionFields: List[SubscriptionField] =
    List(
      GroupEdit,
      ObservationEdit,
      ProgramEdit,
      TargetEdit,
    )

  lazy val SubscriptionMapping =
    ObjectMapping(tpe = SubscriptionType, fieldMappings = subscriptionFields.map(_.FieldMapping))

  lazy val SubscriptionElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    subscriptionFields.foldMap(mf => Map(SubscriptionType -> mf.Elaborator))

  // Convenience for constructing a Subscription stream and corresponding 1-arg elaborator.
  private trait SubscriptionField {
    def Elaborator: PartialFunction[Select, Result[Query]]
    def FieldMapping: RootStream
  }
  private object SubscriptionField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => Stream[F, Result[Query]]) =
      new SubscriptionField {
        val FieldMapping =
          RootStream.computeQuery(fieldName) { (query, tpe, env) =>
            query match
              case Environment(a, Select(b, c, q)) =>
                Nested(env.getR[I]("input").flatTraverse(f(_, q)))
                  .map(q => Environment(a, Select(b, c, q)))
                  .value
              case _ =>
                Result.failure(s"Unexpected: $query").pure[Stream[F, *]]
          }
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
        .map(e => Result(
          Environment(
            Env("editType" -> e.editType),
            Unique(Filter(Predicates.programEdit.value.id.eql(e.programId), child))
          )
        ))
    }

  private val ObservationEdit =
    SubscriptionField("observationEdit", ObservationEditInput.Binding.Option) { (input, child) =>
      topics
        .observation
        .subscribe(1024)
        .filter { e =>
          e.canRead(user) &&
          input.flatMap(_.programId).forall(_ === e.programId) &&
          input.flatMap(_.observationId).forall(_ === e.observationId)
        }
        .map { e =>
          Result(
            Environment(
              Env("editType" -> e.editType),
              Unique(Filter(Predicates.observationEdit.value.id.eql(e.observationId), child))
            )
          )
        }
    }

  private val TargetEdit =
    SubscriptionField("targetEdit", TargetEditInput.Binding.Option) { (input, child) =>
      topics
        .target
        .subscribe(1024)
        .filter { e =>
          e.canRead(user) &&
          input.flatMap(_.programId).forall(_ === e.programId) &&
          input.flatMap(_.targetId).forall(_ === e.targetId)
        }
        .map { e =>
          Result(
            Environment(
              Env("editType" -> e.editType),
              Unique(Filter(Predicates.targetEdit.value.id.eql(e.targetId), child))
            )
          )
        }
    }

  private val GroupEdit =
    SubscriptionField("groupEdit", GroupEditInput.Binding.Option) { (input, child) =>
      topics
        .group
        .subscribe(1024)
        .filter { e =>
          e.canRead(user) &&
          input.flatMap(_.programId).forall(_ === e.programId) &&
          input.flatMap(_.groupId).forall(_ === e.groupId)
        }
        .map { e =>
          Result(
            Environment(
              Env("editType" -> e.editType),
              Unique(Filter(Predicates.groupEdit.value.id.eql(e.groupId), child))
            )
          )
        }
    }

}

