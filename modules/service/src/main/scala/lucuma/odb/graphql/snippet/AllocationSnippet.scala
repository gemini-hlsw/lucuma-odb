// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value.ObjectValue
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.snippet.input.DurationInput
import lucuma.odb.graphql.util._
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AllocationService.SetAllocationResponse.NotAuthorized
import lucuma.odb.service.AllocationService.SetAllocationResponse.PartnerNotFound
import lucuma.odb.service.AllocationService.SetAllocationResponse.ProgramNotFound
import lucuma.odb.service.AllocationService.SetAllocationResponse.Success
import lucuma.odb.util.Codecs._
import skunk.Session
import skunk.codec.temporal.interval

import java.time.Duration

import Bindings._

object AllocationSnippet {

  def apply[F[_]: MonadCancelThrow](
    m: SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
  ): m.Snippet = {
    import m.{ ObjectMapping, Snippet, TableDef, SqlField, SqlRoot, SqlObject, Mutation, MutationCompanionOps, CursorField, col, schema }

    val pool = sessionPool.map(AllocationService.fromSessionAndUser(_, user))

    val AllocationType = schema.ref("Allocation")
    val MutationType   = schema.ref("Mutation")
    val DurationType   = schema.ref("Duration")

    object Allocation extends TableDef("t_allocation") {
      val ProgramId = col("c_program_id", program_id)
      val Partner = col("c_partner", tag)
      val Duration = col("c_duration", interval)
    }

    val setAllocation: Mutation =
      Mutation.simple { (child, env) =>
        ( env.get[Program.Id]("programId"),
          env.get[Tag]("partner"),
          env.get[Duration]("duration")
        ).mapN { (pid, p, d) =>
          pool.use(_.setAllocation(pid, p, d)).map[Result[Query]] {
            case NotAuthorized(user)      => Result.failure(s"User ${user.id} is not authorized to perform this action")
            case PartnerNotFound(_)       => ???
            case ProgramNotFound(_)       => ???
            case Success                  =>
              Result(Unique(Filter(And(
                Eql(UniquePath(List("programId")), Const(pid)),
                Eql(UniquePath(List("partner")), Const(p)),
              ), child)))
          }
        } getOrElse Result.failure(s"Implementation error: expected 'programId', 'partner', and 'duration' in $env.").pure[F].widen
      }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = AllocationType,
          fieldMappings = List(
            SqlField("programId", Allocation.ProgramId, key = true, hidden = true),
            SqlField("partner", Allocation.Partner, key = true),
            SqlObject("duration"),
          )
        ),
        ObjectMapping(
          tpe = DurationType,
          fieldMappings = List(
            SqlField("programId", Allocation.ProgramId, key = true, hidden = true),
            SqlField("partner", Allocation.Partner, key = true, hidden = true),
            SqlField("data", Allocation.Duration, hidden = true),
            CursorField[Long]("microseconds", c => c.fieldAs[Duration]("data").map(_.toMillis * 1000), List("data")),
            CursorField[BigDecimal]("milliseconds", c => c.fieldAs[Duration]("data").map(d => BigDecimal(d.toMillis)), List("data")),
            CursorField[BigDecimal]("seconds", c => c.fieldAs[Duration]("data").map(d => BigDecimal(d.toMillis) / BigDecimal(1000)), List("data")),
            CursorField[BigDecimal]("minutes", c => c.fieldAs[Duration]("data").map(d => BigDecimal(d.toMillis) / BigDecimal(60 * 1000)), List("data")),
            CursorField[BigDecimal]("hours", c => c.fieldAs[Duration]("data").map(d => BigDecimal(d.toMillis) / BigDecimal(60 * 60 * 1000)), List("data")),
          )
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("setAllocation", mutation = setAllocation)
          )
        ),
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {

        case Select("setAllocation", List(
          Query.Binding("input", ObjectValue(List(
            ProgramIdBinding("programId", rProgramId),
            TypedEnumBinding("partner", rPartner),
            DurationInput.Binding("duration", rDuration),
          )))
        ), child) =>
          (rProgramId, rPartner, rDuration).mapN { (pid, enumValue, duration) =>
            Environment(
              Env(
                "programId" -> pid,
                "partner"   -> Tag(enumValue.name.toLowerCase),
                "duration"  -> duration,
              ),
              Select("setAllocation", Nil, child)
            )
          }
      }
    )

    Snippet(typeMappings, elaborator)

  }


}