// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Context
import grackle.Env
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.Term
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import lucuma.core.model.Access
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.ProgramReference
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.AddDatasetEventInput
import lucuma.odb.graphql.input.AddSequenceEventInput
import lucuma.odb.graphql.input.AddStepEventInput
import lucuma.odb.graphql.input.AddTimeChargeCorrectionInput
import lucuma.odb.graphql.input.CloneObservationInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.ConditionsEntryInput
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.CreateProgramInput
import lucuma.odb.graphql.input.CreateTargetInput
import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.graphql.input.LinkUserInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.RecordAtomInput
import lucuma.odb.graphql.input.RecordDatasetInput
import lucuma.odb.graphql.input.RecordGmosStepInput
import lucuma.odb.graphql.input.RecordGmosVisitInput
import lucuma.odb.graphql.input.RedeemUserInvitationInput
import lucuma.odb.graphql.input.RevokeUserInvitationInput
import lucuma.odb.graphql.input.SetAllocationInput
import lucuma.odb.graphql.input.UpdateAsterismsInput
import lucuma.odb.graphql.input.UpdateDatasetsInput
import lucuma.odb.graphql.input.UpdateGroupsInput
import lucuma.odb.graphql.input.UpdateObsAttachmentsInput
import lucuma.odb.graphql.input.UpdateObservationsInput
import lucuma.odb.graphql.input.UpdateProgramsInput
import lucuma.odb.graphql.input.UpdateTargetsInput
import lucuma.odb.graphql.predicate.DatasetPredicates
import lucuma.odb.graphql.predicate.ExecutionEventPredicates
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.service.DatasetService
import lucuma.odb.service.ExecutionEventService
import lucuma.odb.service.GroupService
import lucuma.odb.service.ProgramService
import lucuma.odb.service.SequenceService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.TargetService
import lucuma.odb.service.TargetService.CloneTargetResponse
import lucuma.odb.service.TargetService.UpdateTargetsResponse
import lucuma.odb.service.TargetService.UpdateTargetsResponse.TrackingSwitchFailed
import org.tpolecat.typename.TypeName
import skunk.AppliedFragment
import skunk.Transaction

import scala.reflect.ClassTag

trait MutationMapping[F[_]] extends Predicates[F] {

  private lazy val mutationFields: List[MutationField] =
    List(
      AddConditionsEntry,
      AddDatasetEvent,
      AddSequenceEvent,
      AddStepEvent,
      AddTimeChargeCorrection,
      CloneObservation,
      CloneTarget,
      CreateGroup,
      CreateObservation,
      CreateProgram,
      CreateTarget,
      CreateUserInvitation,
      LinkUser,
      RecordAtom,
      RecordDataset,
      RecordGmosNorthStep,
      RecordGmosNorthVisit,
      RecordGmosSouthStep,
      RecordGmosSouthVisit,
      RedeemUserInvitation,
      RevokeUserInvitation,
      SetAllocation,
      UpdateAsterisms,
      UpdateDatasets,
      UpdateGroups,
      UpdateObsAttachments,
      UpdateObservations,
      UpdatePrograms,
      UpdateTargets,
    )

  lazy val MutationMapping: ObjectMapping =
    ObjectMapping(tpe = MutationType, fieldMappings = mutationFields.map(_.FieldMapping))

  lazy val MutationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    mutationFields.foldMap(_.elaborator)

  // Resources defined in the final cake.
  def services: Resource[F, Services[F]]
  def user: User

  // Convenience for constructing a SqlRoot and corresponding 1-arg elaborator.
  private trait MutationField {
    def elaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]]
    def FieldMapping: RootEffect
  }
  private object MutationField {
    def apply[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: (I, Query) => F[Result[Query]]) =
      new MutationField {
        val FieldMapping =
          RootEffect.computeChild(fieldName) { (child, _, _) =>
            child match {
              case Environment(env, child2) =>
                Nested(env.getR[I]("input").flatTraverse(i => f(i, child2)))
                  .map(child3 => Environment(env, child3))
                  .value
              case _ =>
                Result.internalError(s"Unexpected: $child").pure[F]
            }
          }
        val elaborator =
          case (MutationType, `fieldName`, List(inputBinding("input", rInput))) =>
            Elab.transformChild { child =>
              rInput.map(input => Environment(Env("input" -> input), child))
            }
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

  private def selectPid(
    pid: Option[Program.Id],
    ref: Option[ProgramReference]
  )(using Services[F]): F[Result[Program.Id]] =
    (pid, ref) match {
      case (None, None)    => Result.failure("One of programId or programReference must be provided.").pure[F]
      case (Some(p), None) => p.success.pure[F]
      case (_, Some(r))    => programService.selectPid(r).map { op =>
        op.fold(Result.failure(s"Program reference '${r.format}' was not found.")) { selectedPid =>
          pid.fold(selectedPid.success) { givenPid =>
            Result
              .failure(s"Program reference ${r.format} (id $selectedPid) doesn't correspond to specified program id $givenPid")
              .unlessA(selectedPid === givenPid)
              .as(selectedPid)
          }
        }
      }
    }

  def datasetResultSubquery(dids: List[Dataset.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate       = Predicates.dataset.id.in(dids),
      order           = OrderSelection[Dataset.Id](DatasetType / "id"),
      limit           = limit,
      collectionField = "datasets",
      child
    )

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
        OdbError.NotAuthorized(user.id, Some(s"This action is restricted to staff users.")).asFailureF
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

  private lazy val AddTimeChargeCorrection: MutationField =
    MutationField("addTimeChargeCorrection", AddTimeChargeCorrectionInput.Binding) { (input, child) =>
      services.useTransactionally {
        timeAccountingService.addCorrection(input.visitId, input.correction).as {
          Result(
            Filter(Predicates.addTimeChargeCorrectionResult.timeChargeInvoice.id.eql(input.visitId), child)
          )
        }
      }
    }

  private lazy val CloneObservation: MutationField =
    MutationField("cloneObservation", CloneObservationInput.Binding) { (input, child) =>
      services.useTransactionally {

        val clone: ResultT[F, (Program.Id, Observation.Id)] = 
          ResultT(observationService.cloneObservation(input))

        // this will do nothing if input.asterism is Absent
        def setAsterism(pid: Program.Id, oid: Observation.Id): ResultT[F, Unit] =
          ResultT(asterismService.setAsterism(pid, NonEmptyList.of(oid), input.asterism))

        val doTheThing: F[Result[Observation.Id]] =
          clone.flatMap { (pid, oid) => setAsterism(pid, oid).as(oid) } .value

        doTheThing.map { r =>
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
          case NoSuchTarget(targetId) => OdbError.InvalidTarget(targetId, Some(s"No such target: $targetId")).asFailure
          case UpdateFailed(problem)  =>
            problem match
              case SourceProfileUpdatesFailed(ps) => Result.Failure(ps.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
              case TrackingSwitchFailed(p)        => OdbError.UpdateFailed(Some(p)).asFailure

        }
      }
    }

  private lazy val CreateGroup: MutationField =
    MutationField("createGroup", CreateGroupInput.Binding) { (input, child) =>
      services.useTransactionally {
        ResultT(selectPid(input.programId, input.programReference)).flatMap { pid =>
          ResultT(groupService.createGroup(pid, input.SET).map { gid =>
            Result(Unique(Filter(Predicates.group.id.eql(gid), child)))
          })
        }.value
      }
    }

  private lazy val CreateObservation: MutationField =
    MutationField("createObservation", CreateObservationInput.Binding) { (input, child) =>
      services.useTransactionally {

        def createObservation(pid: Program.Id): F[Result[(Observation.Id, Query)]] =
          observationService.createObservation(pid, input.SET.getOrElse(ObservationPropertiesInput.Create.Default)).map(
            _.fproduct(id => Unique(Filter(Predicates.observation.id.eql(id), child)))
          )

        def insertAsterism(pid: Program.Id, oid: Observation.Id): F[Result[Unit]] =
          input.asterism.toOption.traverse { a =>
            asterismService.insertAsterism(pid, NonEmptyList.one(oid), a)
          }.map(_.getOrElse(Result.unit))

        val query = for {
          pid <- ResultT(selectPid(input.programId, input.programReference))
          tup <- ResultT(createObservation(pid))
          (oid, query) = tup
          _   <- ResultT(insertAsterism(pid, oid))
        } yield query

        for {
          rQuery <- query.value
          _      <- transaction.rollback.unlessA(rQuery.hasValue)
        } yield rQuery
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
        ResultT(selectPid(input.programId, input.programReference)).flatMap { pid =>
          ResultT(targetService.createTarget(pid, input.SET).map {
            case NotAuthorized(user)  => OdbError.NotAuthorized(user.id).asFailure
            case ProgramNotFound(pid) => OdbError.InvalidProgram(pid, Some(s"Program ${pid} was not found")).asFailure
            case Success(id)          => Result(Unique(Filter(Predicates.target.id.eql(id), child)))
          })
        }.value
      }
    }

  private lazy val CreateUserInvitation =
    MutationField("createUserInvitation", CreateUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService.createUserInvitation(input).map: rInv =>
          rInv.map: inv =>
            Environment(
              Env("inv" -> inv), 
              Unique(Filter(Predicates.userInvitation.id.eql(inv.id), child))
            )

  private lazy val LinkUser =
    MutationField("linkUser", LinkUserInput.Binding) { (input, child) =>
      services.useTransactionally {
        import lucuma.odb.service.ProgramService.LinkUserResponse._
        programService.linkUser(input).map[Result[Query]] {
          case NotAuthorized(user)     => OdbError.NotAuthorized(user.id).asFailure
          case AlreadyLinked(pid, uid) => OdbError.NoAction(Some(s"User $uid is already linked to program $pid.")).asFailure
          case InvalidUser(uid)        => OdbError.InvalidUser(uid, Some(s"User $uid does not exist or is of a nonstandard type.")).asFailure
          case Success(pid, uid)       =>
            Result(Unique(Filter(And(
              Predicates.linkUserResult.programId.eql(pid),
              Predicates.linkUserResult.userId.eql(uid),
            ), child)))
        }
      }
    }

  private def recordDatasetResponseToResult(
    child:        Query,
    predicates:   DatasetPredicates
  ): DatasetService.InsertDatasetResponse => Result[Query] = {
    import DatasetService.InsertDatasetResponse.*
    (response: DatasetService.InsertDatasetResponse) => response match {
      case NotAuthorized(user)      =>
        OdbError.NotAuthorized(user.id).asFailure
      case ReusedFilename(filename) =>
        OdbError.InvalidFilename(filename, Some(s"The filename '${filename.format}' is already assigned")).asFailure
      case StepNotFound(id)         =>
        OdbError.InvalidStep(id, Some(s"Step id '$id' not found")).asFailure
      case Success(did, _, _)       =>
        Result(Unique(Filter(predicates.id.eql(did), child)))
    }
  }

  private lazy val RecordDataset: MutationField =
    MutationField("recordDataset", RecordDatasetInput.Binding) { (input, child) =>
      services.useTransactionally {
        datasetService
          .insertDataset(input.stepId, input.filename, input.qaState)
          .map(recordDatasetResponseToResult(child, Predicates.recordDatasetResult.dataset))
      }
    }

  private def executionEventResponseToResult(
    child:        Query,
    predicates:   ExecutionEventPredicates,    
  )(using Services[F]): ExecutionEventService.InsertEventResponse => Result[Query] = {
    import ExecutionEventService.InsertEventResponse.*
    (response: ExecutionEventService.InsertEventResponse) => response match {
      case NotAuthorized(user) => OdbError.NotAuthorized(user.id).asFailure
      case DatasetNotFound(id) => OdbError.InvalidDataset(id, Some(s"Dataset '${id.show}' not found")).asFailure
      case StepNotFound(id)    => OdbError.InvalidStep(id, Some(s"Step '$id' not found")).asFailure
      case VisitNotFound(id)   => OdbError.InvalidVisit(id, Some(s"Visit '$id' not found")).asFailure
      case Success(e)          => Result(Unique(Filter(predicates.id.eql(e.id), child)))
    }
  }

  private def addEvent[I: ClassTag: TypeName](
    fieldName: String,
    matcher:   Matcher[I],
    pred:      ExecutionEventPredicates
  )(
    insert:    I => (Transaction[F], Services[F]) ?=> F[ExecutionEventService.InsertEventResponse]
  ): MutationField =
    MutationField(fieldName, matcher) { (input, child) =>
      services.useTransactionally {
        for {
          r <- insert(input)
          _ <- r.asSuccess.traverse_(s => timeAccountingService.update(s.event.visitId))
        } yield executionEventResponseToResult(child, pred)(r)
      }
    }

  private lazy val AddDatasetEvent: MutationField =
    addEvent("addDatasetEvent", AddDatasetEventInput.Binding, Predicates.datasetEvent) { input =>
      executionEventService.insertDatasetEvent(input.datasetId, input.datasetStage)
    }

  private lazy val AddSequenceEvent: MutationField =
    addEvent("addSequenceEvent", AddSequenceEventInput.Binding, Predicates.sequenceEvent) { input =>
      executionEventService.insertSequenceEvent(input.visitId, input.command)
    }

  private lazy val AddStepEvent: MutationField =
    addEvent("addStepEvent", AddStepEventInput.Binding, Predicates.stepEvent) { input =>
      executionEventService.insertStepEvent(input.stepId, input.stepStage)
    }

  private def recordAtom(
    response:  F[SequenceService.InsertAtomResponse],
    predicate: LeafPredicates[Atom.Id],
    child:     Query
  )(using Services[F]): F[Result[Query]] = {
    import SequenceService.InsertAtomResponse.*
    response.map[Result[Query]] {
      case NotAuthorized(user)           =>
        OdbError.NotAuthorized(user.id).asFailure
      case VisitNotFound(id, instrument) =>
        OdbError.InvalidVisit(id, Some(s"Visit '$id' not found or is not a ${instrument.longName} visit")).asFailure
      case Success(aid)                  =>
        Result(Unique(Filter(predicate.eql(aid), child)))
    }
  }

  private lazy val RecordAtom: MutationField =
    MutationField("recordAtom", RecordAtomInput.Binding) { (input, child) =>
      services.useTransactionally {
        recordAtom(
          sequenceService.insertAtomRecord(input.visitId, input.instrument, input.stepCount, input.sequenceType),
          Predicates.atomRecord.id,
          child
        )
      }
    }

  private def recordStep(
    response:  F[SequenceService.InsertStepResponse],
    predicate: LeafPredicates[Step.Id],
    child:     Query
  ): F[Result[Query]] = {
    import SequenceService.InsertStepResponse.*
    response.map[Result[Query]] {
      case NotAuthorized(user)           =>
        OdbError.NotAuthorized(user.id).asFailure
      case AtomNotFound(id, instrument)  =>
        OdbError.InvalidAtom(id, Some(s"Atom '$id' not found or is not a ${instrument.longName} atom")).asFailure
      case Success(sid)                  =>
        Result(Unique(Filter(predicate.eql(sid), child)))
    }
  }

  private lazy val RecordGmosNorthStep: MutationField =
    MutationField("recordGmosNorthStep", RecordGmosStepInput.GmosNorthBinding) { (input, child) =>
      services.useTransactionally {
        recordStep(
          sequenceService.insertGmosNorthStepRecord(input.atomId, input.instrument, input.step, input.observeClass),
          Predicates.gmosNorthStep.id,
          child
        )
      }
    }

  private lazy val RecordGmosSouthStep: MutationField =
    MutationField("recordGmosSouthStep", RecordGmosStepInput.GmosSouthBinding) { (input, child) =>
      services.useTransactionally {
        recordStep(
          sequenceService.insertGmosSouthStepRecord(input.atomId, input.instrument, input.step, input.observeClass),
          Predicates.gmosSouthStep.id,
          child
        )
      }
    }

  private def recordVisit(
    response:  F[Result[Visit.Id]],
    predicate: LeafPredicates[Visit.Id],
    child:     Query
  )(using Services[F], Transaction[F]): F[Result[Query]] =
    ResultT(response).map(vid => Unique(Filter(predicate.eql(vid), child))).value

  private lazy val RecordGmosNorthVisit: MutationField =
    MutationField("recordGmosNorthVisit", RecordGmosVisitInput.GmosNorthBinding) { (input, child) =>
      services.useTransactionally {
        recordVisit(
          visitService.insertGmosNorth(input.observationId, input.static),
          Predicates.gmosNorthVisit.id,
          child
        )
      }
    }

  private lazy val RecordGmosSouthVisit: MutationField =
    MutationField("recordGmosSouthVisit", RecordGmosVisitInput.GmosSouthBinding) { (input, child) =>
      services.useTransactionally {
        recordVisit(
          visitService.insertGmosSouth(input.observationId, input.static),
          Predicates.gmosSouthVisit.id,
          child
        )
      }
    }

  private lazy val RedeemUserInvitation =
    MutationField("redeemUserInvitation", RedeemUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService.redeemUserInvitation(input).map: rId =>
          rId.map: id =>
            Unique(Filter(Predicates.userInvitation.id.eql(id), child))            

  private lazy val RevokeUserInvitation =
    MutationField("revokeUserInvitation", RevokeUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService.revokeUserInvitation(input).map: rId =>
          rId.map: id =>
            Unique(Filter(Predicates.userInvitation.id.eql(id), child))            

  private lazy val SetAllocation =
    MutationField("setAllocation", SetAllocationInput.Binding): (input, child) =>
      services.useTransactionally:
        ResultT(allocationService.setAllocation(input))
          .as:
            Unique(Filter(And(
              Predicates.setAllocationResult.programId.eql(input.programId),
              Predicates.setAllocationResult.partner.eql(input.partner)
            ), child))
          .value

  // An applied fragment that selects all observation ids that satisfy
  // `filterPredicate`
  private def observationIdSelect(
    includeDeleted: Option[Boolean],
    WHERE:          Option[Predicate]
  ): Result[AppliedFragment] = {
    val whereObservation: Predicate =
      and(List(
        Predicates.observation.program.isWritableBy(user),
        Predicates.observation.existence.includeDeleted(includeDeleted.getOrElse(false)),
        WHERE.getOrElse(True)
      ))
    MappedQuery(
      Filter(whereObservation, Select("id", None, Query.Empty)),
      Context(QueryType, List("observations"), List("observations"), List(ObservationType))
    ).flatMap(_.fragment)
  }

  private lazy val UpdateAsterisms: MutationField =
    MutationField("updateAsterisms", UpdateAsterismsInput.binding(Path.from(ObservationType))) { (input, child) =>
      services.useTransactionally {

        val idSelect: Result[AppliedFragment] =
          observationIdSelect(input.includeDeleted, input.WHERE)

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

  private lazy val UpdateDatasets: MutationField =
    MutationField("updateDatasets", UpdateDatasetsInput.binding(Path.from(DatasetType))) { (input, child) =>
      services.useTransactionally {
        // Our predicate for selecting datasets to update
        val filterPredicate = and(List(
          Predicates.dataset.observation.program.isWritableBy(user),
          input.WHERE.getOrElse(True)
        ))

        val idSelect: Result[AppliedFragment] =
          MappedQuery(Filter(filterPredicate, Select("id", Empty)), Context(QueryType, List("datasets"), List("datasets"), List(DatasetType))).flatMap(_.fragment)

        idSelect.flatTraverse { which =>
          datasetService
            .updateDatasets(input.SET, which)
            .map(datasetResultSubquery(_, input.LIMIT, child))
        }
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
            Filter(filterPredicate, Select("id", Empty)),
            Context(QueryType, List("obsAttachments"), List("obsAttachments"), List(ObsAttachmentType))
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
          observationIdSelect(input.includeDeleted, input.WHERE)

        val updateObservations: F[Result[(Map[Program.Id, List[Observation.Id]], Query)]] =
          idSelect.flatTraverse { which =>
            observationService
              .updateObservations(input.SET, which)
              .map { r =>
                r.flatMap { m =>
                  val oids = m.values.foldLeft(List.empty[Observation.Id])(_ ++ _)
                  observationResultSubquery(oids, input.LIMIT, child).tupleLeft(m)
                }
              }
          }

        def setAsterisms(m: Map[Program.Id, List[Observation.Id]]): F[Result[Unit]] =
          m.toList.traverse { case (pid, oids) =>
            NonEmptyList.fromList(oids).fold(Result.unit.pure[F]) { os =>
              asterismService.setAsterism(pid, os, input.asterism)
            }
          }.map(_.sequence.void)

        val r = for {
          tup <- ResultT(updateObservations)
          _   <- ResultT(setAsterisms(tup._1))
        } yield tup._2

        r.value.flatTap { q => transaction.rollback.unlessA(q.hasValue) }
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
          MappedQuery(Filter(filterPredicate, Select("id", None, Empty)), Context(QueryType, List("programs"), List("programs"), List(ProgramType))).flatMap(_.fragment)

        // Update the specified programs and then return a query for the affected programs.
        idSelect.flatTraverse { which =>
          programService.updatePrograms(input.SET, which)
           .map(
              _.flatMap(programResultSubquery(_, input.LIMIT, child))
            )
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
          MappedQuery(Filter(filterPredicate, Select("id", None, Empty)), Context(QueryType, List("targets"), List("targets"), List(TargetType))).flatMap(_.fragment)

        // Update the specified targets and then return a query for the affected targets (or an error)
        idSelect.flatTraverse { which =>
          targetService.updateTargets(input.SET, which).map {
            case UpdateTargetsResponse.Success(selected)                    => targetResultSubquery(selected, input.LIMIT, child)
            case UpdateTargetsResponse.SourceProfileUpdatesFailed(problems) => Result.Failure(problems.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
            case UpdateTargetsResponse.TrackingSwitchFailed(s)              => OdbError.UpdateFailed(Some(s)).asFailure
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
          MappedQuery(Filter(filterPredicate, Select("id", None, Empty)), Context(QueryType, List("groups"), List("groups"), List(GroupType))).flatMap(_.fragment)

        // Update the specified groups and then return a query for the affected groups (or an error)
        idSelect.flatTraverse { which =>
          import GroupService.UpdateGroupsResponse
          groupService.updateGroups(input.SET, which).map {
            case UpdateGroupsResponse.Success(selected) => groupResultSubquery(selected, input.LIMIT, child)
            case UpdateGroupsResponse.Error(problem)    => OdbError.UpdateFailed(Some(problem)).asFailure
          }
        }

      }
    }

}
