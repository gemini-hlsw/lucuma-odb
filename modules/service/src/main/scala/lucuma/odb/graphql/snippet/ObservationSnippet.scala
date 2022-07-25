// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.snippet.input.CreateObservationInput
import lucuma.odb.graphql.snippet.input.ObservationPropertiesInput
import lucuma.odb.graphql.util._
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ObservationService.InsertObservationResponse.NotAuthorized
import lucuma.odb.service.ObservationService.InsertObservationResponse.Success
import lucuma.odb.util.Codecs._
import skunk.Session

object ObservationSnippet {

  def apply[F[_]: MonadCancelThrow](
    m:    SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    dbPool: Resource[F, Session[F]],
    user: User,
  ): m.Snippet = {

    import m.{ TableDef, ObjectMapping, Join, Snippet, SqlField, SqlObject, Mutation, MutationCompanionOps, SqlRoot, LeafMapping, col, schema }

    // The types that we're going to map.
    // val QueryType         = schema.ref("Query")
    val MutationType      = schema.ref("Mutation")
    // val SubscriptionType  = schema.ref("Subscription")
    val ObservationType   = schema.ref("Observation")
    val ObservationIdType = schema.ref("ObservationId")
    val ObsStatusType     = schema.ref("ObsStatus")
    val ObsActiveStatusType = schema.ref("ObsActiveStatus")
    val CreateObservationResultType = schema.ref("CreateObservationResult")

    val pool = dbPool.map(ObservationService.fromUserAndSession(user, _))

    object Predicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasObservationId(oid: Observation.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

    }

    // Column references for our mapping.
    object ObservationTable extends TableDef("t_observation") {
      val ProgramId  = col("c_program_id", program_id)
      val Id         = col("c_observation_id", observation_id)
      val Existence  = col("c_existence", existence)
      val Name       = col("c_name", text_nonempty.opt)
      val Instrument = col("c_instrument", tag.opt)
      val Status     = col("c_status", obs_status)
      val ActiveStatus = col("c_active_status", obs_active_status)
    }

    // Column references for our mapping.
    object ProgramTable extends TableDef("t_program") {
      val Id        = col("c_program_id", program_id)
    }

    def uniqueObservationNoFiltering(id: Observation.Id, child: Query): Result[Query] =
      Result(Unique(Filter(Predicates.hasObservationId(id), child)))

    val insertObservation: Mutation =
      Mutation.simple { (child, env) =>
        env.getR[CreateObservationInput]("input").flatTraverse { input =>
          pool.use { svc =>
            svc.insertObservation(input.programId, input.SET.getOrElse(ObservationPropertiesInput.DefaultCreate)).map {
              case NotAuthorized(user) => Result.failure(s"User ${user.id} is not authorized to perform this action")
              case Success(id)         => uniqueObservationNoFiltering(id, child)
            }
          }
        }
      }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = ObservationType,
          fieldMappings = List(
            SqlField("id", ObservationTable.Id, key = true),
            SqlField("programId", ObservationTable.ProgramId, hidden=true),
            SqlField("existence", ObservationTable.Existence, hidden = true),
            SqlField("subtitle", ObservationTable.Name),
            SqlField("status", ObservationTable.Status),
            SqlField("activeStatus", ObservationTable.ActiveStatus),
            SqlObject("program", Join(ObservationTable.ProgramId, ProgramTable.Id)),
          ),
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("createObservation", mutation = insertObservation),
          )
        ),
        ObjectMapping(
          tpe = CreateObservationResultType,
          fieldMappings = List(
            SqlField("id", ObservationTable.Id, key = true, hidden = true),
            SqlObject("observation"),
          )
        ),
        LeafMapping[Observation.Id](ObservationIdType),
        LeafMapping[ObsStatus](ObsStatusType),
        LeafMapping[ObsActiveStatus](ObsActiveStatusType),
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {
        case Select("createObservation", List(CreateObservationInput.Binding("input", rInput)), child) =>
          rInput.map { input =>
            Environment(Env("input" -> input), Select("createObservation", Nil, child))
          }
      },
    )

    // Done.
    Snippet(typeMappings, elaborator)

  }


}
