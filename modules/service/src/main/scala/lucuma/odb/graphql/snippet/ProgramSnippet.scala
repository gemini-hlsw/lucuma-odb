package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryInterpreter
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax._
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import lucuma.core.model
import lucuma.core.model.Access._
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.graphql.util._
import lucuma.odb.service.ProgramService
import lucuma.odb.util.Codecs._
import skunk.Session

object ProgramSnippet {

  def apply[F[_]: MonadCancelThrow](m: SnippetMapping[F] with SkunkMapping[F], sessionPool: Resource[F, Session[F]], user: User): m.Snippet = {
    import m.{ TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, SqlObject, Join, Mutation, LeafMapping }

    val pool = sessionPool.map(ProgramService.fromSession(_))

    val schema =
      schema"""

        type Query {

          "Returns the program with the given id, if any."
          program(
            "Program ID"
            programId: ProgramId!
            "Set to true to include deleted values"
            includeDeleted: Boolean! = false
          ): Program

          # TODO: connection
          programs(
            "(Optional) listing of programs to retrieve (all programs if empty)"
            programIds: [ProgramId!]
            "Set to true to include deleted values"
            includeDeleted: Boolean! = false
          ): [Program!]!

          # Pages through all requested programs (or all programs if no ids are given).
          # programs(
          #   "(Optional) listing of programs to retrieve (all programs if empty)"
          #   programIds: [ProgramId!]
          #
          #   "Retrieve `first` values after the given cursor"
          #   first: Int
          #
          #   "Retrieve values after the one associated with this cursor"
          #   after: Cursor
          #
          #   # Set to true to include deleted values
          #   includeDeleted: Boolean! = false
          # ): ProgramConnection!

        }

        "Program creation parameters"
        input CreateProgramInput {
          name: NonEmptyString
        }

        type Mutation {
          createProgram(
            "Program description"
            input: CreateProgramInput!
          ): Program!
        }

        scalar ProgramId

        type Program {

          "Program ID"
          id: ProgramId!

          #"DELETED or PRESENT"
          existence: Existence!

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
            SqlRoot("program"),
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
        LeafMapping[ProgramUserRole](ProgramUserRoleType),
      )


    // Predicates we use our elaborator.
    object Predicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasProgramId(pid: model.Program.Id): Predicate =
        Eql(UniquePath(List("id")), Const(pid))

      def hasProgramId(pids: List[model.Program.Id]): Predicate =
        In(UniquePath(List("id")), pids)

      def hasProgramId(pids: Option[List[model.Program.Id]]): Predicate =
        pids.fold[Predicate](True)(hasProgramId)

      def isVisibleTo(user: model.User): Predicate =
        user.role.access match {
          // TODO: handle NGO case
          case Guest | Pi    | Ngo     => Eql(UniquePath(List("users", "userId")), Const(user.id))
          case Staff | Admin | Service => True
        }

    }

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {

        case Select("program", List(
          ProgramIdBinding("programId", rPid),
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          (rPid, rIncludeDeleted).parMapN { (pid, includeDeleted) =>
            Select("program", Nil,
              Unique(
                Filter(
                  And(
                    Predicates.hasProgramId(pid),
                    Predicates.includeDeleted(includeDeleted),
                    Predicates.isVisibleTo(user),
                  ),
                  child
                )
              )
            )
          }

        case Select("programs", List(
          ProgramIdBinding.List.NullableOptional("programIds", rOptPids),
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          (rOptPids, rIncludeDeleted).parMapN { (optPids, includeDeleted) =>
            Select("programs", Nil,
              Filter(
                And(
                  Predicates.hasProgramId(optPids),
                  Predicates.includeDeleted(includeDeleted),
                  Predicates.isVisibleTo(user)
                ),
                child
              )
            )
          }

      },
      MutationType -> {

        case Select("createProgram", List(
          Binding("input", ObjectValue(List(
            NonEmptyStringBinding.NullableOptional("name", rName))
          ))
        ), child) =>
          rName.map { oName =>
            Environment(
              Env("name" -> oName),
              Select("createProgram", Nil, child)
            )
          }

      }
    )

    Snippet(schema, typeMappings, elaborator)

  }


}