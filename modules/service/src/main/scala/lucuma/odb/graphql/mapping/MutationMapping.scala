// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Applicative
import cats.data.Ior
import cats.data.Nested
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.all.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path
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
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AsterismService
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ProgramService
import lucuma.odb.service.ProposalService
import lucuma.odb.service.TargetService
import org.tpolecat.typename.TypeName
import skunk.AppliedFragment

import scala.reflect.ClassTag

trait MutationMapping[F[_]] extends Predicates[F] {

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
    def FieldMapping: RootEffect
  }
  private object MutationField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => F[Result[Query]]) =
      new MutationField {
        val FieldMapping =
          RootEffect.computeQuery(fieldName) { (query, tpe, env) =>
            query match {
              case Environment(x, Select(y, z, child)) =>
                Nested(env.getR[I]("input").flatTraverse(i => f(i, child)))
                  .map(q => Environment(x, Select(y, z, q)))
                  .value
              case _ =>
                Result.failure(s"Unexpected: $query").pure[F]
            }
          }
        val Elaborator =
          case Select(`fieldName`, List(inputBinding("input", rInput)), child) =>
            rInput.map(input => Environment(Env("input" -> input), Select(fieldName, Nil, child)))
      }
  }

  // Field definitions

  private lazy val CreateObservation: MutationField =
    MutationField("createObservation", CreateObservationInput.Binding) { (input, child) =>

      val createObservation: F[Result[(Observation.Id, Query)]] =
        observationService.use { svc =>
          svc.createObservation(input.programId, input.SET.getOrElse(ObservationPropertiesInput.Create.Default)).map(
            _.fproduct(id => Unique(Filter(Predicates.observation.id.eql(id), child)))
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

  private lazy val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      programService.use(_.insertProgram(input.SET)).map { id =>
        Result(Unique(Filter(Predicates.program.id.eql(id), child)))
      }
    }

  private lazy val CreateTarget =
    MutationField("createTarget", CreateTargetInput.Binding) { (input, child) =>
      targetService.use { ts =>
        import TargetService.CreateTargetResponse._
        ts.createTarget(input.programId, input.SET).map {
          case NotAuthorized(user)  => Result.failure(s"User ${user.id} is not authorized to perform this action")
          case ProgramNotFound(pid) => Result.failure(s"Program ${pid} was not found")
          case Success(id)          => Result(Unique(Filter(Predicates.target.id.eql(id), child)))
        }
      }
    }

  private lazy val LinkUser =
    MutationField("linkUser", LinkUserInput.Binding) { (input, child) =>
      import lucuma.odb.service.ProgramService.LinkUserResponse._
      programService.use(_.linkUser(input)).map[Result[Query]] {
        case NotAuthorized(user)     => Result.failure(s"User ${user.id} is not authorized to perform this action")
        case AlreadyLinked(pid, uid) => Result.failure(s"User $uid is already linked to program $pid.")
        case InvalidUser(uid)        => Result.failure(s"User $uid does not exist or is of a nonstandard type.")
        case Success(pid, uid)       =>
          Result(Unique(Filter(And(
            Predicates.linkUserResult.programId.eql(pid),
            Predicates.linkUserResult.userId.eql(uid),
          ), child)))
      }
    }

  private lazy val SetAllocation =
    MutationField("setAllocation", SetAllocationInput.Binding) { (input, child) =>
      import AllocationService.SetAllocationResponse._
      allocationService.use(_.setAllocation(input)).map[Result[Query]] {
        case NotAuthorized(user) => Result.failure(s"User ${user.id} is not authorized to perform this action")
        case PartnerNotFound(_)  => ???
        case ProgramNotFound(_)  => ???
        case Success             =>
          Result(Unique(Filter(And(
            Predicates.setAllocationResult.programId.eql(input.programId),
            Predicates.setAllocationResult.partner.eql(input.partner)
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
        Predicates.observation.program.id.eql(programId),
        Predicates.observation.program.isWritableBy(user),
        Predicates.observation.existence.includeDeleted(includeDeleted.getOrElse(false)),
        WHERE.getOrElse(True)
      ))
    MappedQuery(
      Filter(whereObservation, Select("id", Nil, Query.Empty)),
      Cursor.Context(QueryType, List("observations"), List("observations"), List(ObservationType))
    ).map(_.fragment)
  }

    private lazy val UpdateAsterisms: MutationField =
    MutationField("updateAsterisms", UpdateAsterismsInput.binding(Path.from(ObservationType))) { (input, child) =>

      val idSelect: Result[AppliedFragment] =
        observationIdSelect(input.programId, input.includeDeleted, input.WHERE)

      val selectObservations: F[Result[(List[Observation.Id], Query)]] =
        idSelect.traverse { which =>
          observationService.use { svc =>
            svc
              .selectObservations(which)
              .fproduct {
                case Nil => Limit(0, child)
                case ids => Filter(Predicates.observation.id.in(ids), child)
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

  private lazy val UpdateObservations: MutationField =
    MutationField("updateObservations", UpdateObservationsInput.binding(Path.from(ObservationType))) { (input, child) =>

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

                case ids => Filter(Predicates.observation.id.in(ids), child)
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

  private lazy val UpdatePrograms =
    MutationField("updatePrograms", UpdateProgramsInput.binding(Path.from(ProgramType))) { (input, child) =>

      // Our predicate for selecting programs to update
      val filterPredicate = and(List(
        Predicates.program.isWritableBy(user),
        Predicates.program.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
        input.WHERE.getOrElse(True)
      ))

      // An applied fragment that selects all program ids that satisfy `filterPredicate`
      val idSelect: Result[AppliedFragment] =
        MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(QueryType, List("programs"), List("programs"), List(ProgramType))).map(_.fragment)

      // We want to order the returned programs by id
      def orderByPid(child: Query) =
        OrderBy(OrderSelections(List(OrderSelection[Program.Id](ProgramType / "id"))), child)

      // Update the specified programs and then return a query for the affected programs.
      idSelect.flatTraverse { which =>
        programService.use(_.updatePrograms(input.SET, which)).map {
          case Nil  => Result(orderByPid(Limit(0, child)))
          case pids => Result(orderByPid(Filter(Predicates.program.id.in(pids), child)))
        } recover {
          case ProposalService.ProposalUpdateException.CreationFailed =>
            Result.failure("One or more programs has no proposal, and there is insufficient information to create one. To add a proposal all required fields must be specified.")
          case ProposalService.ProposalUpdateException.InconsistentUpdate =>
            Result.failure("The specified edits for proposal class do not match the proposal class for one or more specified programs' proposals. To change the proposal class you must specify all fields for that class.")
        }
      }

    }

}

