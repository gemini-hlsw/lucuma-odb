package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import edu.gemini.grackle.syntax._
import lucuma.odb.service.ProgramService
import skunk.Session
import lucuma.odb.graphql.util._
import lucuma.core.model.User
import natchez.Trace
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.temporal.interval
import lucuma.core.model.Partner
import lucuma.odb.data.Tag
import lucuma.core.model.Program
import java.time.Duration
import cats.syntax.all._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Value
import edu.gemini.grackle.Value.ObjectValue
import edu.gemini.grackle.Cursor.Env
import Bindings._
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AllocationService.SetAllocationResponse.NotAuthorized
import lucuma.odb.service.AllocationService.SetAllocationResponse.PartnerNotFound
import lucuma.odb.service.AllocationService.SetAllocationResponse.ProgramNotFound
import lucuma.odb.service.AllocationService.SetAllocationResponse.Success
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Path.UniquePath

object AllocationSnippet {

  def apply[F[_]: MonadCancelThrow: Trace](
    m: SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
  ): m.Snippet = {
    import m.{ ObjectMapping, Snippet, TableDef, SqlField, SqlRoot, Mutation, MutationCompanionOps, col }

    val pool = sessionPool.map(AllocationService.fromSessionAndUser(_, user))

    val schema =
      schema"""

        type Allocation {
          partner: Partner!
          duration: Duration!
        }

        input SetAllocationInput {
          programId: ProgramId!
          partner: Partner!
          duration: Duration!
        }

        type Mutation {
          "Set the allocation for a program from the specified partner."
          setAllocation(input: SetAllocationInput!): Allocation!
        }

      """

    val AllocationType = schema.ref("Allocation")
    val MutationType   = schema.ref("Mutation")

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
            case PartnerNotFound(partner) => ???
            case ProgramNotFound(pid)     => ???
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
            SqlField("duration", Allocation.Duration),
          )
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("setAllocation", mutation = setAllocation)
          )
        )
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {

        case Select("setAllocation", List(
          Query.Binding("input", ObjectValue(List(
            ProgramIdBinding("programId", rProgramId),
            TypedEnumBinding("partner", rPartner),
            DurationBinding("duration", rDuration),
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

    Snippet(schema, typeMappings, elaborator)

  }


}