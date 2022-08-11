// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.User
import lucuma.odb.graphql.snippet.input.CreateProgramInput
import lucuma.odb.graphql.snippet.input.UpdateProgramsInput
import lucuma.odb.graphql.snippet.predicates.ProgramPredicates
import lucuma.odb.graphql.util.MutationCompanionOps
import lucuma.odb.instances.given
import lucuma.odb.service.ProgramService
import skunk.AppliedFragment
import scala.reflect.ClassTag
import lucuma.odb.graphql.util.Bindings.Matcher
import lucuma.odb.graphql.snippet.input.LinkUserInput
import edu.gemini.grackle.Path.UniquePath
import lucuma.odb.graphql.snippet.input.SetAllocationInput
import lucuma.odb.service.AllocationService
import org.tpolecat.typename.TypeName

trait MutationMapping[F[_]: MonadCancelThrow]
  extends ProgramPredicates[F]
     with MutationCompanionOps[F]
  { this: SkunkMapping[F] =>

  lazy val MutationType = schema.ref("Mutation")

  private lazy val mutationFields: List[MutationField] =
    List(
      CreateProgram,
      LinkUser,
      SetAllocation,
      UpdatePrograms,
    )

  lazy val MutationMapping =
    ObjectMapping(tpe = MutationType, fieldMappings = mutationFields.map(_.FieldMapping))

  lazy val MutationElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    mutationFields.foldMap(mf => Map(MutationType -> mf.Elaborator))

  // Resources needed by mutations
  def allocationService: Resource[F, AllocationService[F]]
  def programService: Resource[F, ProgramService[F]]
  def user: User

  // Convenience for constructing a SqlRoot and corresponding 1-arg elaborator.
  private trait MutationField {
    def Elaborator: PartialFunction[Select, Result[Query]]
    def FieldMapping: SqlRoot
  }
  private object MutationField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => F[Result[Query]]) =
      new MutationField {
        val FieldMapping = SqlRoot(fieldName, mutation = Mutation.simple((child, env) => env.getR[I]("input").flatTraverse(f(_, child))))
        val Elaborator =
          case Select(`fieldName`, List(inputBinding("input", rInput)), child) =>
            rInput.map(input => Environment(Env("input" -> input), Select(fieldName, Nil, child)))
      }
  }

  // Field definitions

  private val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      programService.use(_.insertProgram(input.SET.name)).map { id =>
        Result(Unique(Filter(ProgramPredicates.hasProgramId(id), child)))
      }
    }

  private val LinkUser =
    MutationField("linkUser", LinkUserInput.Binding) { (input, child) =>
      import lucuma.odb.service.ProgramService.LinkUserResponse._
      programService.use(_.linkUser(input)).map[Result[Query]] {
        case NotAuthorized(user)     => Result.failure(s"User ${user.id} is not authorized to perform this action")
        case AlreadyLinked(pid, uid) => Result.failure(s"User $uid is already linked to program $pid.")
        case InvalidUser(uid)        => Result.failure(s"User $uid does not exist or is of a nonstandard type.")
        case Success(pid, uid)       =>
          Result(Unique(Filter(And(
            Eql(UniquePath(List("programId")), Const(pid)),
            Eql(UniquePath(List("userId")), Const(uid)),
          ), child)))
      }
    }

  private val SetAllocation =
    MutationField("setAllocation", SetAllocationInput.Binding) { (input, child) =>
      import AllocationService.SetAllocationResponse._
      allocationService.use(_.setAllocation(input)).map[Result[Query]] {
        case NotAuthorized(user) => Result.failure(s"User ${user.id} is not authorized to perform this action")
        case PartnerNotFound(_)  => ???
        case ProgramNotFound(_)  => ???
        case Success             =>
          Result(Unique(Filter(And(
            Eql(UniquePath(List("programId")), Const(input.programId)),
            Eql(UniquePath(List("partner")), Const(input.partner)),
          ), child)))
      }
    }

  private val UpdatePrograms =
    MutationField("updatePrograms", UpdateProgramsInput.Binding) { (input, child) =>

      // Our predicate for selecting programs to update
      val filterPredicate = and(List(
        ProgramPredicates.isWritableBy(user),
        ProgramPredicates.includeDeleted(input.includeDeleted.getOrElse(false)),
        input.WHERE.getOrElse(True),
      ))

      // An applied fragment that selects all program ids that satisfy `filterPredicate`
      val idSelect: Result[AppliedFragment] =
        Result.fromOption(
          MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(ProgramType)).map(_.fragment),
          "Could not construct a subquery for the provided WHERE condition." // shouldn't happen
        )

      // Update the specified programs and then return a query for the same set of programs.
      idSelect.traverse { which =>
        programService.use(_.updatePrograms(input.SET, which)).as(Filter(filterPredicate, child))
      }

    }

}

