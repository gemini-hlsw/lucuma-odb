// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Applicative
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.CreateProgramInput
import lucuma.odb.graphql.input.CreateTargetInput
import lucuma.odb.graphql.input.LinkUserInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.SetAllocationInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateProgramsInput
import lucuma.odb.graphql.predicates.ObservationPredicates
import lucuma.odb.graphql.predicates.ProgramPredicates
import lucuma.odb.graphql.predicates.TargetPredicates
import lucuma.odb.graphql.util.Bindings.Matcher
import lucuma.odb.graphql.util.MutationCompanionOps
import lucuma.odb.instances.given
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AsterismService
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ProgramService
import lucuma.odb.service.TargetService
import org.tpolecat.typename.TypeName
import skunk.AppliedFragment

import scala.reflect.ClassTag

trait MutationMapping[F[_]: MonadCancelThrow]
  extends ProgramPredicates[F]
     with ObservationPredicates[F]
     with TargetPredicates[F]
     with MutationCompanionOps[F]
  { this: SkunkMapping[F] =>

  lazy val MutationType = schema.ref("Mutation")

  private lazy val mutationFields: List[MutationField] =
    List(
      CreateObservation,
      CreateProgram,
      CreateTarget,
      LinkUser,
      SetAllocation,
      UpdateAsterisms,
      UpdateObservations,
      UpdatePrograms
    )

  lazy val MutationMapping: ObjectMapping =
    ObjectMapping(tpe = MutationType, fieldMappings = mutationFields.map(_.FieldMapping))

  lazy val MutationElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    mutationFields.foldMap(mf => Map(MutationType -> mf.Elaborator))

  // Resources needed by mutations
  def allocationService: Resource[F, AllocationService[F]]
  def asterismService: Resource[F, AsterismService[F]]
  def observationService: Resource[F, ObservationService[F]]
  def programService: Resource[F, ProgramService[F]]
  def targetService: Resource[F, TargetService[F]]
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

  private val CreateObservation: MutationField =
    MutationField("createObservation", CreateObservationInput.Binding) { (input, child) =>

      val createObservation: F[Result[(Observation.Id, Query)]] =
        observationService.use { svc =>
          svc.createObservation(input.programId, input.SET.getOrElse(ObservationPropertiesInput.Default)).map(
            _.fproduct(id => Unique(Filter(ObservationPredicates.hasObservationId(id), child)))
          )
        }

      def insertAsterism(oid: Option[Observation.Id]): F[Result[Unit]] =
        oid.flatTraverse { o =>
          input.asterism.toOption.traverse { a =>
            asterismService.use(_.insertAsterism(input.programId, NonEmptyList.one(o), a))
          }
        }.map(_.getOrElse(Result.unit))

      pool.use { s =>
        s.transaction.use { xa =>
          for {
            rTup  <- createObservation
            oid    = rTup.toOption.map(_._1)
            rUnit <- insertAsterism(oid)
            query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
            // Fail altogether if there was an issue, say, creating the asterism
            _     <- query.left.traverse_(_ => xa.rollback)
          } yield query
        }
      }
    }

  private val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      programService.use(_.insertProgram(input.SET.name)).map { id =>
        Result(Unique(Filter(ProgramPredicates.hasProgramId(id), child)))
      }
    }

  private val CreateTarget =
    MutationField("createTarget", CreateTargetInput.Binding) { (input, child) =>
      targetService.use { ts =>
        import TargetService.CreateTargetResponse._
        ts.createTarget(input.programId, input.SET).map {
          case NotAuthorized(user)  => Result.failure(s"User ${user.id} is not authorized to perform this action")
          case ProgramNotFound(pid) => Result.failure(s"Program ${pid} was not found")
          case Success(id)          => Result(Unique(Filter(TargetPredicates.hasTargetId(id), child)))
        }
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
            Eql(UniquePath(List("userId")), Const(uid))
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
            Eql(UniquePath(List("partner")), Const(input.partner))
          ), child)))
      }
    }

  // An applied fragment that selects all observation ids that satisfy
  // `filterPredicate`
  private def observationIdSelect(
    programId:      Program.Id,
    includeDeleted: Option[Boolean],
    WHERE:          Option[Predicate]
  ): Result[AppliedFragment] = {
    val whereObservation: Predicate =
      and(List(
        Eql(UniquePath(List("program", "id")), Const(programId)),
        ObservationPredicates.isWritableBy(user),
        ObservationPredicates.includeDeleted(includeDeleted.getOrElse(false)),
        WHERE.getOrElse(True)
      ))

    Result.fromOption(
      MappedQuery(
        Filter(whereObservation, Select("id", Nil, Query.Empty)),
        Cursor.Context(ObservationType)
      ).map(_.fragment),
      "Could not construct a subquery for the provided WHERE condition."
    )
  }

    private val UpdateAsterisms: MutationField =
    MutationField("updateAsterisms", UpdateAsterismsInput.Binding) { (input, child) =>

      val idSelect: Result[AppliedFragment] =
        observationIdSelect(input.programId, input.includeDeleted, input.WHERE)

      val selectObservations: F[Result[(List[Observation.Id], Query)]] =
        idSelect.traverse { which =>
          observationService.use { svc =>
            svc
              .selectObservations(which)
              .fproduct {
                case Nil => Limit(0, child)
                case ids => Filter(ObservationPredicates.inObservationIds(ids), child)
              }
          }
        }

      def setAsterisms(oids: List[Observation.Id]): F[Result[Unit]] =
        NonEmptyList.fromList(oids).traverse { os =>
          val add = input.SET.ADD.flatMap(NonEmptyList.fromList)
          val del = input.SET.DELETE.flatMap(NonEmptyList.fromList)
          asterismService.use(_.updateAsterism(input.programId, os, add, del))
        }.map(_.getOrElse(Result.unit))

      pool.use { s =>
        s.transaction.use { xa =>
          for {
            rTup  <- selectObservations
            oids   = rTup.toList.flatMap(_._1)
            rUnit <- setAsterisms(oids)
            query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
            _     <- query.left.traverse_(_ => xa.rollback)
          } yield query
        }
      }
    }

  private val UpdateObservations: MutationField =
    MutationField("updateObservations", UpdateObservationsInput.Binding) { (input, child) =>

      val idSelect: Result[AppliedFragment] =
        observationIdSelect(input.programId, input.includeDeleted, input.WHERE)

      val updateObservations: F[Result[(List[Observation.Id], Query)]] =
        idSelect.flatTraverse { which =>
          observationService.use { svc =>
            svc
              .updateObservations(input.SET, which)
              .map(_.fproduct {

                // When there are no selected ids, return a Query which matches
                // nothing (i.e., no results for the update).

                // "In" Predicate is used in the normal case where there is
                // at least one matching observation id.  But "In" requires a
                // non-empty list (eventually) so we need to catch the Nil
                // case.

                // Produces "Unable to map query":
                // case Nil => Filter(False, child)

                // Work around using a zero limit
                case Nil => Limit(0, child)

                case ids => Filter(ObservationPredicates.inObservationIds(ids), child)
              })
          }
        }

      def setAsterisms(oids: List[Observation.Id]): F[Result[Unit]] =
        NonEmptyList.fromList(oids).traverse { os =>
          asterismService.use(_.setAsterism(input.programId, os, input.asterism))
        }.map(_.getOrElse(Result.unit))

      pool.use { s =>
        s.transaction.use { xa =>
          for {
            rTup  <- updateObservations
            oids   = rTup.toList.flatMap(_._1)
            rUnit <- setAsterisms(oids)
            query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
            _     <- query.left.traverse_(_ => xa.rollback)
          } yield query
        }
      }
    }

  private val UpdatePrograms =
    MutationField("updatePrograms", UpdateProgramsInput.Binding) { (input, child) =>

      // Our predicate for selecting programs to update
      val filterPredicate = and(List(
        ProgramPredicates.isWritableBy(user),
        ProgramPredicates.includeDeleted(input.includeDeleted.getOrElse(false)),
        input.WHERE.getOrElse(True)
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

