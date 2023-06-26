// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import cats.kernel.Order
import cats.syntax.all.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*
import edu.gemini.grackle.Result
import edu.gemini.grackle.Term
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.AddSequenceEventInput
import lucuma.odb.graphql.input.CloneObservationInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.CreateProgramInput
import lucuma.odb.graphql.input.CreateTargetInput
import lucuma.odb.graphql.input.LinkUserInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.RecordGmosNorthStepInput
import lucuma.odb.graphql.input.RecordGmosNorthVisitInput
import lucuma.odb.graphql.input.RecordGmosSouthStepInput
import lucuma.odb.graphql.input.RecordGmosSouthVisitInput
import lucuma.odb.graphql.input.SetAllocationInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateGroupsInput
import lucuma.odb.graphql.input.UpdateObsAttachmentsInput
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateProgramsInput
import lucuma.odb.graphql.input.UpdateProposalAttachmentsInput
import lucuma.odb.graphql.input.UpdateTargetsInput
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.service.AllocationService
import lucuma.odb.service.AsterismService
import lucuma.odb.service.ExecutionEventService
import lucuma.odb.service.GroupService
import lucuma.odb.service.ObsAttachmentMetadataService
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ProgramService
import lucuma.odb.service.ProposalAttachmentMetadataService
import lucuma.odb.service.ProposalService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.StepService
import lucuma.odb.service.TargetService
import lucuma.odb.service.TargetService.CloneTargetResponse
import lucuma.odb.service.TargetService.UpdateTargetsResponse
import lucuma.odb.service.TargetService.UpdateTargetsResponse.TrackingSwitchFailed
import lucuma.odb.service.VisitService
import org.tpolecat.typename.TypeName
import skunk.AppliedFragment
import skunk.Transaction

import scala.reflect.ClassTag
import lucuma.odb.graphql.input.ConditionsEntryInput
import lucuma.core.model.Access

trait MutationMapping[F[_]] extends Predicates[F] {

  private lazy val mutationFields: List[MutationField] =
    List(
      AddConditionsEntry,
      AddSequenceEvent,
      CloneObservation,
      CloneTarget,
      CreateGroup,
      CreateObservation,
      CreateProgram,
      CreateTarget,
      LinkUser,
      RecordGmosNorthStep,
      RecordGmosNorthVisit,
      RecordGmosSouthStep,
      RecordGmosSouthVisit,
      SetAllocation,
      UpdateAsterisms,
      UpdateGroups,
      UpdateObsAttachments,
      UpdateObservations,
      UpdatePrograms,
      UpdateProposalAttachments,
      UpdateTargets,
    )

  lazy val MutationMapping: ObjectMapping =
    ObjectMapping(tpe = MutationType, fieldMappings = mutationFields.map(_.FieldMapping))

  lazy val MutationElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    mutationFields.foldMap(mf => Map(MutationType -> mf.Elaborator))

  // Resources defined in the final cake.
  def services: Resource[F, Services[F]]
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

  def mutationResultSubquery[A: Order](predicate: Predicate, order: OrderSelection[A], limit: Option[NonNegInt], collectionField: String, child: Query): Result[Query] =
    val limitʹ = limit.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
    ResultMapping.mutationResult(child, limitʹ, collectionField) { q =>
      FilterOrderByOffsetLimit(
        pred = Some(predicate),
        oss = Some(List(order)),
        offset = None,
        limit = Some(limitʹ + 1), // Select one extra row here.
        child = q
      )
    }

  def obsAttachmentResultSubquery(aids: List[ObsAttachment.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.obsAttachment.id.in(aids),
      order = OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"),
      limit = limit,
      collectionField = "obsAttachments",
      child
    )

  def observationResultSubquery(oids: List[Observation.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.observation.id.in(oids),
      order = OrderSelection[Observation.Id](ObservationType / "id"),
      limit = limit,
      collectionField = "observations",
      child
    )

  def programResultSubquery(pids: List[Program.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.program.id.in(pids),
      order = OrderSelection[Program.Id](ProgramType / "id"),
      limit = limit,
      collectionField = "programs",
      child
    )

  def proposalAttachmentResultSubquery(pid: Program.Id, aTypes: List[Tag], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = And(Predicates.proposalAttachment.program.id.eql(pid), Predicates.proposalAttachment.attachmentType.in(aTypes)),
      order = OrderSelection[Tag](ProposalAttachmentType / "attachmentType"),
      limit = limit,
      collectionField = "proposalAttachments",
      child
    )

  // Field definitions

  private lazy val AddConditionsEntry: MutationField =
    MutationField("addConditionsEntry", ConditionsEntryInput.Binding) { (input, child) =>
      if user.role.access < Access.Staff then {
        Result.failure(s"This action is restricted to staff users.").pure[F]
      } else {
        services.useTransactionally {
          chronicleService.addConditionsEntry(input).map { id =>
            Result(
              Filter(Predicates.addConditionsEntyResult.conditionsEntry.id.eql(id), child)
            )
          }
        }  
      }
    }

  private lazy val CloneObservation: MutationField =
    MutationField("cloneObservation", CloneObservationInput.Binding) { (input, child) =>
      services.useTransactionally {
        observationService.cloneObservation(input).map { r =>
          r.map { oid =>
            Filter(And(
              Predicates.cloneObservationResult.originalObservation.id.eql(input.observationId),
              Predicates.cloneObservationResult.newObservation.id.eql(oid)
            ), child)
          }
        }
      }
    }

  private lazy val CloneTarget: MutationField =
    import CloneTargetResponse.*
    import UpdateTargetsResponse.{ SourceProfileUpdatesFailed, TrackingSwitchFailed }
    MutationField("cloneTarget", CloneTargetInput.Binding) { (input, child) =>
      services.useTransactionally {
        targetService.cloneTarget(input).map {          

          // Typical case
          case Success(oldTargetId, newTargetId) =>
            Result(
              Filter(And(
                Predicates.cloneTargetResult.originalTarget.id.eql(oldTargetId),
                Predicates.cloneTargetResult.newTarget.id.eql(newTargetId)
              ), child)
            )

          // Failure Cases
          case NoSuchTarget(targetId) => Result.failure(s"No such target: $targetId")
          case UpdateFailed(problem)  =>
            problem match
              case SourceProfileUpdatesFailed(ps) => Result.Failure(ps)
              case TrackingSwitchFailed(p)        => Result.failure(p)

        }
      }
    }

  private lazy val CreateGroup: MutationField =
    MutationField("createGroup", CreateGroupInput.Binding) { (input, child) =>
      services.useTransactionally {
        groupService.createGroup(input).map { gid =>
          Result(Unique(Filter(Predicates.group.id.eql(gid), child)))
        }
      }
    }

  private lazy val CreateObservation: MutationField =
    MutationField("createObservation", CreateObservationInput.Binding) { (input, child) =>
      services.useTransactionally {

        val createObservation: F[Result[(Observation.Id, Query)]] =
          observationService.createObservation(input.programId, input.SET.getOrElse(ObservationPropertiesInput.Create.Default)).map(
            _.fproduct(id => Unique(Filter(Predicates.observation.id.eql(id), child)))
          )

        def insertAsterism(oid: Option[Observation.Id]): F[Result[Unit]] =
          oid.flatTraverse { o =>
            input.asterism.toOption.traverse { a =>
              asterismService.insertAsterism(input.programId, NonEmptyList.one(o), a)
            }
          }.map(_.getOrElse(Result.unit))

        for {
          rTup  <- createObservation
          oid    = rTup.toOption.map(_._1)
          rUnit <- insertAsterism(oid)
          query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
          // Fail altogether if there was an issue, say, creating the asterism
          _     <- transaction.rollback.unlessA(query.hasValue)
        } yield query

      }
    }

  private lazy val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      services.useTransactionally {
        programService.insertProgram(input.SET).map { id =>
          Result(Unique(Filter(Predicates.program.id.eql(id), child)))
        }
      }
    }

  private lazy val CreateTarget =
    MutationField("createTarget", CreateTargetInput.Binding) { (input, child) =>
      services.useTransactionally {
        import TargetService.CreateTargetResponse._
        targetService.createTarget(input.programId, input.SET).map {
          case NotAuthorized(user)  => Result.failure(s"User ${user.id} is not authorized to perform this action")
          case ProgramNotFound(pid) => Result.failure(s"Program ${pid} was not found")
          case Success(id)          => Result(Unique(Filter(Predicates.target.id.eql(id), child)))
        }
      }
    }

  private lazy val LinkUser =
    MutationField("linkUser", LinkUserInput.Binding) { (input, child) =>
      services.useTransactionally {
        import lucuma.odb.service.ProgramService.LinkUserResponse._
        programService.linkUser(input).map[Result[Query]] {
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
    }

  private lazy val AddSequenceEvent: MutationField =
    MutationField("addSequenceEvent", AddSequenceEventInput.Binding) { (input, child) =>
      services.useTransactionally {
        import ExecutionEventService.InsertEventResponse.*
        executionEventService.insertExecutionEvent(input.visitId, input.command).map[Result[Query]] {
          case NotAuthorized(user) =>
            Result.failure(s"User '${user.id}' is not authorized to perform this action")
          case VisitNotFound(id)   =>
            Result.failure(s"Visit id '$id' not found")
          case Success(eid)        =>
            Result(Unique(Filter(Predicates.sequenceEvent.id.eql(eid), child)))
        }
      }
    }

  private def recordStep(
    response:  F[StepService.InsertStepResponse],
    predicate: LeafPredicates[Step.Id],
    child:     Query
  ): F[Result[Query]] = {
    import StepService.InsertStepResponse.*
    response.map[Result[Query]] {
      case NotAuthorized(user)                 =>
        Result.failure(s"User '${user.id}' is not authorized to perform this action")
      case VisitNotFound(id, instrument)       =>
        Result.failure(s"Visit '$id' not found or is not a ${instrument.longName} visit")
      case Success(sid)                        =>
        Result(Unique(Filter(predicate.eql(sid), child)))
    }
  }

  private lazy val RecordGmosNorthStep: MutationField =
    MutationField("recordGmosNorthStep", RecordGmosNorthStepInput.Binding) { (input, child) =>
      services.useTransactionally {
        recordStep(
          stepService.insertGmosNorth(input.visitId, input.instrument, input.step),
          Predicates.gmosNorthStep.id,
          child
        )
      }
    }

  private lazy val RecordGmosSouthStep: MutationField =
    MutationField("recordGmosSouthStep", RecordGmosSouthStepInput.Binding) { (input, child) =>
      services.useTransactionally {
        recordStep(
          stepService.insertGmosSouth(input.visitId, input.instrument, input.step),
          Predicates.gmosSouthStep.id,
          child
        )
      }
    }

  private def recordVisit(
    response:  F[VisitService.InsertVisitResponse],
    predicate: LeafPredicates[Visit.Id],
    child:     Query
  ): F[Result[Query]] = {
    import VisitService.InsertVisitResponse.*
    response.map[Result[Query]] {
      case NotAuthorized(user)                 =>
        Result.failure(s"User '${user.id}' is not authorized to perform this action")
      case ObservationNotFound(id, instrument) =>
        Result.failure(s"Observation '$id' not found or is not a ${instrument.longName} observation")
      case Success(vid)                        =>
        Result(Unique(Filter(predicate.eql(vid), child)))
    }
  }


  private lazy val RecordGmosNorthVisit: MutationField =
    MutationField("recordGmosNorthVisit", RecordGmosNorthVisitInput.Binding) { (input, child) =>
      services.useTransactionally {
        recordVisit(
          visitService.insertGmosNorth(input.observationId, input.static),
          Predicates.gmosNorthVisit.id,
          child
        )
      }
    }

  private lazy val RecordGmosSouthVisit: MutationField =
    MutationField("recordGmosSouthVisit", RecordGmosSouthVisitInput.Binding) { (input, child) =>
      services.useTransactionally {
        recordVisit(
          visitService.insertGmosSouth(input.observationId, input.static),
          Predicates.gmosSouthVisit.id,
          child
        )
      }
    }

  private lazy val SetAllocation =
    MutationField("setAllocation", SetAllocationInput.Binding) { (input, child) =>
      import AllocationService.SetAllocationResponse._
      services.useTransactionally {
        allocationService.setAllocation(input).map[Result[Query]] {
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
    ).flatMap(_.fragment)
  }

  private lazy val UpdateAsterisms: MutationField =
    MutationField("updateAsterisms", UpdateAsterismsInput.binding(Path.from(ObservationType))) { (input, child) =>
      services.useTransactionally {

        val idSelect: Result[AppliedFragment] =
          observationIdSelect(input.programId, input.includeDeleted, input.WHERE)

        val selectObservations: F[Result[(List[Observation.Id], Query)]] =
          idSelect.traverse { which =>
            observationService.selectObservations(which)            
          } map { r => 
            r.flatMap { oids => 
              observationResultSubquery(oids, input.LIMIT, child)
                .tupleLeft(oids)
            }
          }

        def setAsterisms(oids: List[Observation.Id]): F[Result[Unit]] =
          NonEmptyList.fromList(oids).traverse { os =>
            val add = input.SET.ADD.flatMap(NonEmptyList.fromList)
            val del = input.SET.DELETE.flatMap(NonEmptyList.fromList)
            asterismService.updateAsterism(input.programId, os, add, del)
          }.map(_.getOrElse(Result.unit))

        for {
          rTup  <- selectObservations
          oids   = rTup.toList.flatMap(_._1)
          rUnit <- setAsterisms(oids)
          query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
          _     <- transaction.rollback.unlessA(query.hasValue)
        } yield query

      }
    }

  private lazy val UpdateObsAttachments =
      MutationField("updateObsAttachments", UpdateObsAttachmentsInput.binding(Path.from(ObsAttachmentType))) { (input, child) =>
        services.useTransactionally {
          val filterPredicate = and(List(
            Predicates.obsAttachment.program.id.eql(input.programId),
            Predicates.obsAttachment.program.isWritableBy(user),
            input.WHERE.getOrElse(True)
          ))

          val idSelect: Result[AppliedFragment] = 
            MappedQuery(
              Filter(filterPredicate, Select("id", Nil, Empty)), 
              Cursor.Context(QueryType, List("obsAttachments"), List("obsAttachments"), List(ObsAttachmentType))
            ).flatMap(_.fragment)

          idSelect.flatTraverse { which =>
            obsAttachmentMetadataService.updateObsAttachments(input.SET, which).map(obsAttachmentResultSubquery(_, input.LIMIT, child))
          }
        }
      }

  private lazy val UpdateObservations: MutationField =
    MutationField("updateObservations", UpdateObservationsInput.binding(Path.from(ObservationType))) { (input, child) =>
      services.useTransactionally {

        val idSelect: Result[AppliedFragment] =
          observationIdSelect(input.programId, input.includeDeleted, input.WHERE)

        val updateObservations: F[Result[(List[Observation.Id], Query)]] =
          idSelect.flatTraverse { which =>
            observationService
              .updateObservations(input.programId, input.SET, which)
              .map { r => 
                r.flatMap { oids => 
                  observationResultSubquery(oids, input.LIMIT, child)
                    .tupleLeft(oids)
                }
              }
          }

        def setAsterisms(oids: List[Observation.Id]): F[Result[Unit]] =
          NonEmptyList.fromList(oids).traverse { os =>
            asterismService.setAsterism(input.programId, os, input.asterism)
          }.map(_.getOrElse(Result.unit))

        for {
          rTup  <- updateObservations
          oids   = rTup.toList.flatMap(_._1)
          rUnit <- setAsterisms(oids)
          query  = (rTup, rUnit).parMapN { case ((_, query), _) => query }
          _     <- transaction.rollback.unlessA(query.hasValue)
        } yield query

      }
    }

  private lazy val UpdatePrograms =
    MutationField("updatePrograms", UpdateProgramsInput.binding(Path.from(ProgramType))) { (input, child) =>
      services.useTransactionally {
        // Our predicate for selecting programs to update
        val filterPredicate = and(List(
          Predicates.program.isWritableBy(user),
          Predicates.program.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
          input.WHERE.getOrElse(True)
        ))

        // An applied fragment that selects all program ids that satisfy `filterPredicate`
        val idSelect: Result[AppliedFragment] =
          MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(QueryType, List("programs"), List("programs"), List(ProgramType))).flatMap(_.fragment)

        // Update the specified programs and then return a query for the affected programs.
        idSelect.flatTraverse { which =>
          programService.updatePrograms(input.SET, which).map(programResultSubquery(_, input.LIMIT, child)).recover {
            case ProposalService.ProposalUpdateException.CreationFailed =>
              Result.failure("One or more programs has no proposal, and there is insufficient information to create one. To add a proposal all required fields must be specified.")
            case ProposalService.ProposalUpdateException.InconsistentUpdate =>
              Result.failure("The specified edits for proposal class do not match the proposal class for one or more specified programs' proposals. To change the proposal class you must specify all fields for that class.")
          }
        }
      }
    }

  private lazy val UpdateProposalAttachments =
      MutationField("updateProposalAttachments", UpdateProposalAttachmentsInput.binding(Path.from(ProposalAttachmentType))) { (input, child) =>
        services.useTransactionally {

          val filterPredicate = and(List(
            Predicates.proposalAttachment.program.id.eql(input.programId),
            Predicates.proposalAttachment.program.isWritableBy(user),
            input.WHERE.getOrElse(True)
          ))

          val typeSelect: Result[AppliedFragment] = 
            MappedQuery(
              Filter(filterPredicate, Select("attachmentType", Nil, Empty)), 
              Cursor.Context(QueryType, List("proposalAttachments"), List("proposalAttachments"), List(ProposalAttachmentType))
            ).flatMap(_.fragment)

          typeSelect.flatTraverse { which =>
            proposalAttachmentMetadataService
              .updateProposalAttachments(input.SET, which)
              .map(proposalAttachmentResultSubquery(input.programId, _, input.LIMIT, child))
          }
          
        }
      }

  def targetResultSubquery(pids: List[Target.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate = Predicates.target.id.in(pids),
      order = OrderSelection[Target.Id](TargetType / "id"),
      limit = limit,
      collectionField = "targets",
      child
    )

  private lazy val UpdateTargets =
    MutationField("updateTargets", UpdateTargetsInput.binding(Path.from(TargetType))) { (input, child) =>
      services.useTransactionally {

        // Our predicate for selecting targets to update
        val filterPredicate = and(List(
          Predicates.target.program.isWritableBy(user),
          Predicates.target.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
          input.WHERE.getOrElse(True)
        ))

        // An applied fragment that selects all target ids that satisfy `filterPredicate`
        val idSelect: Result[AppliedFragment] =
          MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(QueryType, List("targets"), List("targets"), List(TargetType))).flatMap(_.fragment)

        // Update the specified targets and then return a query for the affected targets (or an error)
        idSelect.flatTraverse { which =>
          targetService.updateTargets(input.SET, which).map {
            case UpdateTargetsResponse.Success(selected)                    => targetResultSubquery(selected, input.LIMIT, child)
            case UpdateTargetsResponse.SourceProfileUpdatesFailed(problems) => Result.Failure(problems)
            case UpdateTargetsResponse.TrackingSwitchFailed(problem)        => Result.failure(problem)
          }
        }

      }
    }

  def groupResultSubquery(pids: List[Group.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate = Predicates.group.id.in(pids),
      order = OrderSelection[Group.Id](GroupType / "id"),
      limit = limit,
      collectionField = "groups",
      child
    )

  private lazy val UpdateGroups =
    MutationField("updateGroups", UpdateGroupsInput.binding(Path.from(GroupType))) { (input, child) =>
      services.useTransactionally {

        // Our predicate for selecting groups to update
        val filterPredicate = and(List(
          // TODO: Predicates.group.program.isWritableBy(user),
          input.WHERE.getOrElse(True)
        ))

        // An applied fragment that selects all group ids that satisfy `filterPredicate`
        val idSelect: Result[AppliedFragment] =
          MappedQuery(Filter(filterPredicate, Select("id", Nil, Empty)), Cursor.Context(QueryType, List("groups"), List("groups"), List(GroupType))).flatMap(_.fragment)

        // Update the specified groups and then return a query for the affected groups (or an error)
        idSelect.flatTraverse { which =>
          import GroupService.UpdateGroupsResponse
          groupService.updateGroups(input.SET, which).map {
            case UpdateGroupsResponse.Success(selected) => groupResultSubquery(selected, input.LIMIT, child)
            case UpdateGroupsResponse.Error(problem)    => Result.failure(problem)
          }
        }

      }
    }

}
