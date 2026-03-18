// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.data.Nested
import cats.syntax.all.*
import fs2.Stream
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.OdbMapping.Topics
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.input.ConfigurationRequestEditInput
import lucuma.odb.graphql.input.DatasetEditInput
import lucuma.odb.graphql.input.ExecutionEventAddedInput
import lucuma.odb.graphql.input.GroupEditInput
import lucuma.odb.graphql.input.ObscalcUpdateInput
import lucuma.odb.graphql.input.ObservationEditInput
import lucuma.odb.graphql.input.ProgramEditInput
import lucuma.odb.graphql.input.TargetEditInput
import lucuma.odb.graphql.mapping.ResultMapping.mapSomeFields
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
      DatasetEdit,
      ExecutionEventAdded,
      GroupEdit,
      ObscalcUpdate,
      ObservationEdit,
      ProgramEdit,
      TargetEdit,
      ConfigurationRequestEdit
    )

  lazy val SubscriptionMapping =
    ObjectMapping(SubscriptionType)(subscriptionFields.map(_.FieldMapping)*)

  lazy val SubscriptionElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    subscriptionFields.foldMap(_.elaborator)

  // Convenience for constructing a Subscription stream and corresponding 1-arg elaborator.
  private trait SubscriptionField {
    def elaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]
    def FieldMapping: RootStream
  }
  private object SubscriptionField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => Stream[F, Result[Query]]) =
      new SubscriptionField {
        val FieldMapping =
          RootStream.computeChild(fieldName) { (child, _, _) =>
            child match
              case Environment(env, child2) =>
                Nested(env.getR[I]("input").flatTraverse(f(_, child2)))
                  .map(child3 => Environment(env, child3))
                  .value
              case _ =>
                Result.internalError(s"Unexpected: $child").pure[Stream[F, *]]
          }
        val elaborator =
          case (SubscriptionType, `fieldName`, List(inputBinding("input", rInput))) =>
            Elab.transformChild { child =>
              rInput.map(input => Environment(Env("input" -> input), child))
            }
      }
  }

  private val DatasetEdit =
    SubscriptionField("datasetEdit", DatasetEditInput.Binding.Option): (input, child) =>
      topics
        .dataset
        .subscribe(1024)
        .filter: e =>
          e.canRead(user)                                              &&
          input.flatMap(_.programId).forall(_ === e.programId)         &&
          input.flatMap(_.observationId).forall(_ === e.observationId) &&
          input.flatMap(_.datasetId).forall(_ === e.datasetId)         &&
          input.flatMap(_.isWritten).forall(_ === e.isWritten)
        .map(e => Result(
          Environment(
            Env(
              "editType"  -> e.editType,
              "datasetId" -> e.datasetId
            ),
            Unique(Filter(Predicates.datasetEdit.value.id.eql(e.datasetId), child))
          )
        ))

  private val ExecutionEventAdded =
    SubscriptionField("executionEventAdded", ExecutionEventAddedInput.Binding.Option): (input, child) =>
      topics
        .executionEvent
        .subscribe(1024)
        .filter: e =>
          e.canRead(user)                                              &&
          input.flatMap(_.programId).forall(_ === e.programId)         &&
          input.flatMap(_.observationId).forall(_ === e.observationId) &&
          input.flatMap(_.visitId).forall(_ === e.visitId)             &&
          input.flatMap(_.eventType).forall(_.matches(e.eventType))
        .map: e =>
          Unique(Filter(Predicates.executionEventAdded.executionEventId.eql(e.exectionEventId), child)).success

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

  private val ObscalcUpdate =
    SubscriptionField("obscalcUpdate", ObscalcUpdateInput.Binding.Option): (input, child) =>
      topics
        .obscalc
        .subscribe(1024)
        .filter: e =>
          e.canRead(user)                                              &&
          input.flatMap(_.programId).forall(_ === e.programId)         &&
          input.flatMap(_.observationId).forall(_ === e.observationId) &&
          input.flatMap(_.oldCalculationState).forall(_.matches(e.oldState)) &&
          input.flatMap(_.newCalculationState).forall(_.matches(e.newState))
        .map: e =>
          Result(
            Environment(
              Env(
                "editType"      -> e.editType,
                "observationId" -> e.observationId,
                "oldState"      -> e.oldState,
                "newState"      -> e.newState
              ),
              Filter(
                Predicates.obscalcUpdate.programId.eql(e.programId),
                Query.mapSomeFields(child):
                  case Select("value", a, c) =>
                    Select("value", a,
                      // This predicate needs to be down here
                      Filter(
                        if e.editType === EditType.HardDelete
                        then Predicates.observation.id.isNull(true)
                        else Predicates.observation.id.eql(e.observationId),
                        c
                      )
                    )
              )
            )
          )

  private val ObservationEdit =
    SubscriptionField("observationEdit", ObservationEditInput.Binding.Option) { (input, child) =>
      topics
        .observation
        .subscribe(1024)
        .filter { e =>
          e.canRead(user) && ((
            input.flatMap(_.programId).forall(_ === e.programId) &&
            input.flatMap(_.observationId).forall(_ === e.observationId)
          ))
        }
        .map { e =>
          Result(
            Environment(
              Env(
                "editType" -> e.editType,
                "observationId" -> e.observationId,
              ),
              Filter(
                Predicates.observationEdit.programId.eql(e.programId),
                Query.mapSomeFields(child):
                  case Select("value", a, c) =>
                    Select("value", a,
                      // This predicate needs to be down here
                      Filter(
                        if e.editType === EditType.HardDelete
                        then Predicates.observation.id.isNull(true) // always false; Predicate.False doesn't work
                        else Predicates.observation.id.eql(e.observationId),
                        c
                      )
                    )
              )
            )
          )
        }
    }

  private val ConfigurationRequestEdit =
    SubscriptionField("configurationRequestEdit", ConfigurationRequestEditInput.Binding.Option) { (input, child) =>
      topics
        .configurationRequest
        .subscribe(1024)
        .filter { e =>
          e.canRead(user) && ((
            input.flatMap(_.programId).forall(_ === e.programId)
          ))
        }
        .map { e =>
          Result(
            Environment(
              Env(
                "editType" -> e.editType,
                "configurationRequestId" -> e.configurationRequestId,
              ),
              Filter(
                Predicates.observationEdit.programId.eql(e.programId),
                Query.mapSomeFields(child):
                  case Select("configurationRequest", a, c) =>
                    Select("configurationRequest", a,
                      // This predicate needs to be down here
                      Filter(
                        if e.editType === EditType.HardDelete
                        then Predicates.configurationRequest.id.isNull(true) // always false; Predicate.False doesn't work
                        else Predicates.configurationRequest.id.eql(e.configurationRequestId),
                        c
                      )
                    )
              )
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
              Env(
                "editType" -> e.editType,
                "targetId" -> e.targetId,
              ),
              Filter(
                Predicates.targetEdit.programId.eql(e.programId),
                Query.mapSomeFields(child):
                  case Select("value", a, c) =>
                    Select("value", a,
                      // This predicate needs to be down here
                      Filter(
                        if e.editType === EditType.HardDelete
                        then Predicates.target.id.isNull(true) // always false; Predicate.False doesn't work
                        else Predicates.target.id.eql(e.targetId),
                        c
                      )
                    )
              )
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
          {
            input match
              case None    => true
              case Some(i) =>
                i.groupId match
                  case Nullable.Absent       => true
                  case Nullable.Null         => e.groupId.isEmpty
                  case Nullable.NonNull(gid) => e.groupId.exists(_ == gid)
          }
        }
        .map { e =>

          // We need to burrow down into the query to add a filter to the `value` selection to handle
          // the case where it may be null (in which case we add a filter that will never match).
          val lastGroupId = Group.Id.fromLong(Long.MaxValue).get // will never match
          val predicate   = Predicates.group.id.eql(e.groupId.getOrElse(lastGroupId))
          def addValueFilter(q: Query): Query =
            q match
              case Select("value", a, c) => Select("value", a, Unique(Filter(predicate, c)))
              case Query.Group(queries)  => Query.Group(queries.map(addValueFilter))
              case c                     => c

          Result(
            Environment(
              Env("editType" -> e.editType, "programId" -> e.programId),
              Unique(
                Filter(
                  Predicates.groupEdit.program.id.eql(e.programId),
                  addValueFilter(child)
                )
              )
            )
          )

        }
    }

}
