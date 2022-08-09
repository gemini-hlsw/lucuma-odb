// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path._
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value._
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import io.circe.Encoder
import lucuma.core.model
import lucuma.core.model.Access._
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data._
import lucuma.odb.graphql.snippet.input.CreateProgramInput
import lucuma.odb.graphql.snippet.input.UpdateProgramsInput
import lucuma.odb.graphql.snippet.input.WhereProgram
import lucuma.odb.graphql.util.Bindings._
import lucuma.odb.graphql.util._
import lucuma.odb.service.ProgramService
import lucuma.odb.service.ProgramService.LinkUserResponse._
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk.AppliedFragment
import skunk.Session
import skunk.codec.all._

import java.time.Duration

object ProgramSnippet {

  def apply[F[_]: MonadCancelThrow: Trace](
    m: SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
    topics: OdbMapping.Topics[F],
  ): m.Snippet = {

    import m.{ MappedQuery, ColumnRef, CursorField, PrefixedMapping, TableDef, ObjectMapping, Snippet, SqlRoot, SqlField, SqlObject, Join, Mutation, LeafMapping, MutationCompanionOps, col, schema }

    val pool = sessionPool.map(ProgramService.fromSessionAndUser(_, user))

    // The types that we're going to map.
    val QueryType                = schema.ref("Query")
    val MutationType             = schema.ref("Mutation")
    val SubscriptionType         = schema.ref("Subscription")
    val ProgramType              = schema.ref("Program")
    val ProgramUserType          = schema.ref("ProgramUser")
    val ProgramUserRoleType      = schema.ref("ProgramUserRole")
    val ProgramIdType            = schema.ref("ProgramId")
    val UserIdType               = schema.ref("UserId")
    val CreateProgramResultType  = schema.ref("CreateProgramResult")
    val LinkUserResultType       = schema.ref("LinkUserResult")
    val PlannedTimeSummaryType   = schema.ref("PlannedTimeSummary")
    val NonNegDurationType       = schema.ref("NonNegDuration")

    // Column references for our mapping.
    object Program extends TableDef("t_program") {
      val Id        = col("c_program_id", program_id)
      val PiUserId  = col("c_pi_user_id", user_id)
      val Existence = col("c_existence", existence)
      val Name      = col("c_name", text_nonempty.opt)
      object PlannedTime {
        val Pi        = col("c_pts_pi", interval)
        val Uncharged = col("c_pts_uncharged", interval)
        val Execution   = col("c_pts_execution", interval)
      }
    }
    object ProgramUser extends TableDef("t_program_user") {
      val ProgramId = col("c_program_id", program_id)
      val UserId    = col("c_user_id", user_id)
      val Role      = col("c_role", program_user_role)
    }
    object User extends TableDef("t_user") {
      val Id = col("c_user_id", user_id)
    }
    object ObservationView extends TableDef("v_observation") {
      val ProgramId: ColumnRef    = col("c_program_id",          program_id)
    }

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
          case Guest | Pi =>
            Or(
              Contains(ListPath(List("users", "userId")), Const(user.id)), // user is linked, or
              Eql(UniquePath(List("piUserId")), Const(user.id))            // user is the PI
            )
          case Ngo => ???
          case Staff | Admin | Service => True
        }

      def isWritableBy(user: model.User): Predicate =
        isVisibleTo(user) // this is true for now

    }

    /**
     * Query for a specific program without any filtering, for cases where we already know the
     * user is allowed to see the program in question.
     */
    def uniqueProgramNoFiltering(id: model.Program.Id, child: Query): Result[Query] =
      Result(Unique(Filter(Predicates.hasProgramId(id), child)))

    def createProgram: Mutation =
      Mutation.simple { (child, env) =>
        env.get[Option[NonEmptyString]]("name").map { name =>
          pool.use(_.insertProgram(name)).map(uniqueProgramNoFiltering(_, child))
        } getOrElse Result.failure(s"Implementation error: expected 'name' in $env.").pure[F].widen
      }

    def updatePrograms: Mutation =
      Mutation.simple { (child, env) =>
        env.getR[UpdateProgramsInput]("input").flatTraverse { input =>

          // Our predicate for selecting programs to update
          val filterPredicate = and(List(
            Predicates.isWritableBy(user),
            Predicates.includeDeleted(input.includeDeleted.getOrElse(false)),
            input.WHERE.getOrElse(True),
          ))

          // An applied fragment that selects all program ids that satisfy `filterPredicate`
          val idSelect: Result[AppliedFragment] =
            Result.fromOption(
             MappedQuery(Filter(filterPredicate, Select("id", Nil, Query.Empty)), Cursor.Context(ProgramType)).map(_.fragment),
              "Could not construct a subquery for the provided WHERE condition." // shouldn't happen
            )

          // Update the specified programs and then return a query for the same set of programs.
          idSelect.traverse { which =>
            pool.use(_.updatePrograms(input.SET, which)).as(Filter(filterPredicate, child))
          }

        }
      }

    def linkUser: Mutation =
      Mutation.simple { (child, env) =>
        env.get[ProgramService.LinkUserRequest]("req").map { req =>
          pool.use(_.linkUser(req)).map[Result[Query]] {
            case NotAuthorized(user)     => Result.failure(s"User ${user.id} is not authorized to perform this action")
            case AlreadyLinked(pid, uid) => Result.failure(s"User $uid is already linked to program $pid.")
            case InvalidUser(uid)        => Result.failure(s"User $uid does not exist or is of a nonstandard type.")
            case Success(pid, uid)       =>
              Result(Unique(Filter(And(
                Eql(UniquePath(List("programId")), Const(pid)),
                Eql(UniquePath(List("userId")), Const(uid)),
              ), child)))
          }
        } getOrElse Result.failure(s"Implementation error: expected 'req' in $env.").pure[F].widen
      }

    def programEdit: Mutation =
      Mutation.simpleStream { (child, env) =>
        env.get[Option[model.Program.Id]]("programId").map { pid =>
          topics
            .program
            .subscribe(1024)
            .filter(e => e.canRead(user) && pid.forall(_ === e.programId))
            .map(e => uniqueProgramNoFiltering(e.programId, child))
        } getOrElse Result.failure(s"Implementation error: expected 'programId' in $env.").pure[Stream[F, *]].widen
      }

    // Our mapping, finally.
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
            SqlRoot("updatePrograms", mutation = updatePrograms),
            SqlRoot("linkUser", mutation = linkUser),
          )
        ),
        ObjectMapping(
          tpe = SubscriptionType,
          fieldMappings = List(
            SqlRoot("programEdit", mutation = programEdit)
          )
        ),
        ObjectMapping(
          tpe = ProgramType,
          fieldMappings = List(
            SqlField("id", Program.Id, key = true),
            SqlField("existence", Program.Existence, hidden = true),
            SqlField("name", Program.Name),
            SqlField("piUserId", Program.PiUserId, hidden = true),
            SqlObject("pi", Join(Program.PiUserId, User.Id)),
            SqlObject("users", Join(Program.Id, ProgramUser.ProgramId)),
            SqlObject("plannedTime"),
            SqlObject("observations", Join(Program.Id, ObservationView.ProgramId)),
          ),
        ),
        ObjectMapping(
          tpe = PlannedTimeSummaryType,
          fieldMappings = List(
            SqlField("id", Program.Id, key = true, hidden = true),
            SqlObject("pi"),
            SqlObject("execution"),
            SqlObject("uncharged"),
          ),
        ), {

        def valueAs[A: Encoder](name: String)(f: Duration => A): CursorField[A] =
            CursorField[A](name, c => c.fieldAs[Duration]("value").map(f), List("value"))

        def nonNegDurationMapping(col: ColumnRef): ObjectMapping =
          ObjectMapping(
            tpe = NonNegDurationType,
            fieldMappings = List(
              SqlField("id", Program.Id, key = true, hidden = true),
              SqlField("value", col, hidden = true),
              valueAs("microseconds")(d => d.toMillis * 1000L),
              valueAs("milliseconds")(d => BigDecimal(d.toMillis)),
              valueAs("seconds")(d => BigDecimal(d.toMillis) / BigDecimal(1000)),
              valueAs("minutes")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60)),
              valueAs("hours")(d => BigDecimal(d.toMillis) / BigDecimal(1000 * 60 * 60)),
              valueAs("iso")(d => d.toString),
            ),
          )

        PrefixedMapping(
          tpe = NonNegDurationType,
          mappings = List(
            List("plannedTime", "pi")        -> nonNegDurationMapping(Program.PlannedTime.Pi),
            List("plannedTime", "uncharged") -> nonNegDurationMapping(Program.PlannedTime.Uncharged),
            List("plannedTime", "execution") -> nonNegDurationMapping(Program.PlannedTime.Execution),
          ),
        )

        },
        ObjectMapping(
          tpe = CreateProgramResultType,
          fieldMappings = List(
            SqlField("id", Program.Id, key = true),
            SqlObject("program"),
          )
        ),
        ObjectMapping(
          tpe = LinkUserResultType,
          fieldMappings = List(
            SqlField("programId", ProgramUser.ProgramId, hidden = true, key = true),
            SqlField("userId", ProgramUser.UserId, key = true),
            SqlObject("user"),
          )
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

    // And the elaborator itself. A case for each query/mutation.
    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {

        case Select("program", List(
          ProgramIdBinding("programId", rPid),
        ), child) =>
          rPid.map { pid =>
            Select("program", Nil,
              Unique(
                Filter(
                  And(
                    Predicates.hasProgramId(pid),
                    Predicates.isVisibleTo(user),
                  ),
                  child
                )
              )
            )
          }

        case Select("programs", List(
          WhereProgram.Binding.Option("WHERE", rWHERE),
          ProgramIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parMapN { (WHERE, OFFSET, LIMIT, includeDeleted) =>
            Select("programs", Nil,
              Limit(
                LIMIT.foldLeft(1000)(_ min _.value),
                Filter(
                  And.all(
                    OFFSET.map(pid => GtEql(UniquePath(List("id")), Const(pid))).getOrElse(True),
                    Predicates.includeDeleted(includeDeleted),
                    Predicates.isVisibleTo(user),
                    WHERE.getOrElse(True)
                  ),
                  child
                )
              )
            )
          }

      },
      ProgramType -> {

        case Select("observations", List(
          BooleanBinding("includeDeleted", rIncludeDeleted),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
        ), child) =>
          (rIncludeDeleted, rOFFSET, rLIMIT).parMapN { (includeDeleted, OFFSET, _) =>
            Select("observations", Nil,
              Filter(and(List(
                if (includeDeleted) True else Eql[Existence](UniquePath(List("existence")), Const(Existence.Present)),
                OFFSET.fold[Predicate](True)(o => GtEql[model.Observation.Id](UniquePath(List("id")), Const(o))),
              )),
              child
              )
            )
          }

      },
      MutationType -> {

        case Select("createProgram", List(
          CreateProgramInput.Binding("input", rInput)
        ), child) =>
          rInput.map { input =>
            Environment(
              Env("name" -> input.SET.name),
              Select("createProgram", Nil, child)
            )
          }

        case Select("updatePrograms", List(
          UpdateProgramsInput.Binding("input", rInput)
        ), child) =>
          rInput.map { input =>
            Environment(
              Env("input" -> input),
              Select("updatePrograms", Nil, child)
            )
          }

        case Select("linkUser", List(
          Binding("input", ObjectValue(List(
            ProgramIdBinding("programId", rProgramId),
            UserIdBinding("userId", rUserId),
            ProgramUserRoleBinding("role", rRole),
            ProgramUserSupportRoleTypeBinding.Option("supportType", rSupportType),
            TagBinding.Option("supportPartner", rPartner),
          )))
        ), child) =>
          (rProgramId, rUserId, rRole, rSupportType, rPartner).mapN { (pid, uid, role, tpe, tag) =>
            ProgramService.LinkUserRequest.validate(pid, uid, role, tpe, tag) match {
              case Left(err)  => Result.failure(err)
              case Right(req) => Result(req)
            }
          } .flatten.map { req =>
            Environment(
              Env("req" -> req),
              Select("linkUser", Nil, child)
            )
          }

      },
      SubscriptionType -> {

        case Select("programEdit", List(
          ProgramIdBinding.Option("programId", rPid)
        ), child) =>
          rPid.map { oPid =>
            Environment(
              Env(
                "programId" -> oPid
              ),
              Select("programEdit", Nil, child)
            )
          }

      }
    )

    // Done.
    Snippet(typeMappings, elaborator)

  }


}