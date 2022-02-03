package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryInterpreter
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax._
import fs2.Stream
import lucuma.odb.util.Codecs._
import skunk.Session
import lucuma.core.model.User
import lucuma.core.model.Access._
import io.circe.Encoder
import org.tpolecat.typename.TypeName
import org.tpolecat.sourcepos.SourcePos
import lucuma.odb.service.ProgramService
import lucuma.odb.graphql.util._
import eu.timepit.refined.types.string.NonEmptyString

object ProgramSnippet {

  def apply[F[_]: MonadCancelThrow](m: SnippetMapping[F] with SkunkMapping[F], sessionPool: Resource[F, Session[F]], user: User): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, SqlObject, Join, Mutation, LeafMapping }

    val pool = sessionPool.map(ProgramService.fromSession(_))

    val schema =
      schema"""

        type Query {
          programs: [Program!]!
        }

        type Mutation {
          createProgram(name: NonEmptyString): Program!
        }

        scalar ProgramId

        type Program {

          "Program ID"
          id: ProgramId!

          #"DELETED or PRESENT"
          #existence: Existence!

          "Program name"
          name: NonEmptyString

          #"All observations associated with the program (needs pagination)."
          #observations(
          #  "Retrieve `first` values after the given cursor"
          #  first: Int
          #
          #  "Retrieve values after the one associated with this cursor"
          #  after: Cursor
          #
          #  "Set to true to include deleted values"
          #  includeDeleted: Boolean! = false
          #): ObservationConnection!

          #"Program planned time calculation."
          #plannedTime(
          #  "Set to true to include deleted values"
          #  includeDeleted: Boolean! = false
          #): PlannedTimeSummary!

          "Users associated with this science program"
          users:   [ProgramUser!]!

        }

        enum ProgramUserRole {
          PI
          COI
          OBSERVER
          SUPPORT
        }

        type ProgramUser {
          role:   ProgramUserRole!
          userId: UserId!
          user:   User!
        }
      """

    val QueryType           = schema.ref("Query")
    val MutationType        = schema.ref("Mutation")
    val ProgramType         = schema.ref("Program")
    val ProgramUserType     = schema.ref("ProgramUser")
    val ProgramUserRoleType = schema.ref("ProgramUserRole")
    val ProgramIdType       = schema.ref("ProgramId")
    val UserIdType          = schema.ref("UserId")

    object Program extends TableDef("t_program") {
      val Id        = col("c_program_id", program_id)
      val Existence = col("c_existence", existence)
      val Name      = col("c_name", text_nonempty.opt)
    }

    object ProgramUser extends TableDef("t_program_user") {
      val ProgramId = col("c_program_id", program_id)
      val UserId    = col("c_user_id", user_id)
      val Role      = col("c_role", program_user_role)
    }

    object User extends TableDef("t_user") {
      val Id = col("c_user_id", user_id)
    }

    def createProgram: Mutation =
      Mutation { (child, env) =>
        env.get[Option[NonEmptyString]]("name") match {
          case None =>
            QueryInterpreter.mkErrorResult[(Query, Cursor.Env)](s"Implementation error: expected 'name' in $env.").pure[Stream[F,*]]
          case Some(name) =>
            Stream.eval {
              pool.use { s =>
                s.insertProgram(name, user.id).map { id =>
                  (Unique(Filter(Eql(UniquePath(List("id")), Const(id)), child)), env).rightIor
                }
              }
            }
          }
      }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            SqlRoot("programs"),
          )
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("createProgram", mutation = createProgram),
          )
        ),
        ObjectMapping(
          tpe = ProgramType,
          fieldMappings = List(
            SqlField("id", Program.Id, key = true),
            SqlField("existence", Program.Existence, hidden = true),
            SqlField("name", Program.Name),
            SqlObject("users", Join(Program.Id, ProgramUser.ProgramId))
          ),
        ),
        ObjectMapping(
          tpe = ProgramUserType,
          fieldMappings = List(
            SqlField("programId", ProgramUser.ProgramId, hidden = true, key = true),
            SqlField("userId", ProgramUser.UserId, key = true),
            SqlField("role", ProgramUser.Role),
            SqlObject("user", Join(ProgramUser.UserId, User.Id))
          ),
        ),
        LeafMapping[lucuma.core.model.User.Id](UserIdType),
        LeafMapping[lucuma.core.model.Program.Id](ProgramIdType),
        LeafMapping(ProgramUserRoleType)(TypeName.typeName[String], Encoder[String].contramap[String](_.toUpperCase), SourcePos.instance),
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {
        case Select("programs", Nil, child) =>
          Select("programs", Nil,
            Filter(
              user.role.access match {

                // Guests and PIs can see their own programs
                case Guest | Pi | Ngo /* TODO */ =>
                  And(
                    Eql(UniquePath(List("existence")), Const(true)),
                    Eql(UniquePath(List("users", "userId")), Const(user.id))
                  )

                // Staff, Admin, and Service users can see everything
                case Staff | Admin | Service =>
                  Eql(UniquePath(List("existence")), Const(true))

              },
              child
            )
          ).rightIor
      },
      MutationType -> {

        case Select("createProgram", List(Binding("name", Value.StringValue(name))), child) =>
          NonEmptyString.from(name) match {
            case Left(_)     => Result.failure("'name' may not be empty.")
            case Right(name) =>
              Environment(
                Cursor.Env("name" -> Some(name)),
                Select("createProgram", Nil, child)
              ).rightIor
          }

        case Select("createProgram", List(Binding("name", Value.AbsentValue | Value.NullValue)), child) =>
          Environment(
            Cursor.Env("name" -> None),
            Select("createProgram", Nil, child)
          ).rightIor

      }
    )

    Snippet(schema, typeMappings, elaborator)

  }


}