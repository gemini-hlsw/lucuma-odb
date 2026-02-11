// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Env
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.Result.Failure
import grackle.Result.Warning
import grackle.ResultT
import grackle.Term
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Attachment
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.core.model.ProgramUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.OdbErrorExtensions.asFailureF
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.predicate.DatasetPredicates
import lucuma.odb.graphql.predicate.ExecutionEventPredicates
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.instances.given
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.Services.Syntax.*
import org.http4s.client.Client
import org.tpolecat.typename.TypeName
import skunk.SqlState
import skunk.Transaction

import scala.reflect.ClassTag

trait MutationMapping[F[_]] extends AccessControl[F] {

  private lazy val mutationFields: List[MutationField] =
    List(
      AddConditionsEntry,
      AddAtomEvent,
      AddDatasetEvent,
      AddProgramUser,
      AddSequenceEvent,
      AddSlewEvent,
      AddStepEvent,
      AddTimeChargeCorrection,
      ChangeProgramUserRole,
      CloneGroup,
      CloneObservation,
      CloneTarget,
      CreateCallForProposals,
      CreateConfigurationRequest,
      CreateGroup,
      CreateObservation,
      CreateProgram,
      CreateProgramNote,
      CreateProposal,
      CreateTarget,
      CreateUserInvitation,
      DeleteProgramUser,
      DeleteProposal,
      LinkUser,
      RecordDataset,
      RecordFlamingos2Visit,
      RecordGmosNorthVisit,
      RecordGmosSouthVisit,
      RedeemUserInvitation,
      ResetAcquisition,
      RevokeUserInvitation,
      SetAllocations,
      SetGuideTargetName,
      SetObservationWorkflowState,
      SetProgramReference,
      SetProposalStatus,
      UnlinkUser,
      UpdateAsterisms,
      UpdateAttachments,
      UpdateCallsForProposals,
      UpdateConfigurationRequests,
      UpdateDatasets,
      UpdateGroups,
      UpdateObservations,
      UpdateObservationsTimes,
      UpdateProgramNotes,
      UpdatePrograms,
      UpdateProgramUsers,
      UpdateProposal,
      UpdateTargets,
    )

  lazy val MutationMapping: ObjectMapping =
    ObjectMapping(MutationType)(mutationFields.map(_.FieldMapping)*)

  lazy val MutationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    mutationFields.foldMap(_.elaborator)

  // Resources defined in the final cake.
  def services: Resource[F, Services[F]]
  def user: User
  def httpClient: Client[F]
  def gaiaClient: GaiaClient[F]
  def itcClient: ItcClient[F]
  def emailConfig: Config.Email

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

    /** A mutation that yields a Json result. */
    def json[I: ClassTag: TypeName](fieldName: String, inputBinding: Matcher[I])(f: I => F[Result[Json]]) =
      new MutationField {
        val FieldMapping =
          RootEffect.computeJson(fieldName): (_, env) =>
            Nested(env.getR[I]("input").flatTraverse(i => f(i))).value
        val elaborator =
          case (MutationType, `fieldName`, List(inputBinding("input", rInput))) =>
            Elab.liftR(rInput).flatMap: i =>
              Elab.env("input" -> i)
      }

    def encodable[I: ClassTag: TypeName, A: io.circe.Encoder](fieldName: String, inputBinding: Matcher[I])(f: I => F[Result[A]]) =
      json(fieldName, inputBinding)(i => f(i).map(_.map(_.asJson)))

  }

  def mutationResultSubquery[A](predicate: Predicate, order: OrderSelection[A], limit: Option[NonNegInt], collectionField: String, child: Query): Result[Query] =
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

  def callForProposalsResultSubquery(cids: List[CallForProposals.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate       = Predicates.callForProposals.id.in(cids),
      order           = OrderSelection[CallForProposals.Id](CallForProposalsType / "id"),
      limit           = limit,
      collectionField = "callsForProposals",
      child
    )

  def configurationRequestResultSubquery(ids: List[ConfigurationRequest.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate       = Predicates.configurationRequest.id.in(ids),
      order           = OrderSelection[ConfigurationRequest.Id](ConfigurationRequestType / "id"),
      limit           = limit,
      collectionField = "requests",
      child
    )

  def datasetResultSubquery(dids: List[Dataset.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate       = Predicates.dataset.id.in(dids),
      order           = OrderSelection[Dataset.Id](DatasetType / "id"),
      limit           = limit,
      collectionField = "datasets",
      child
    )

  def attachmentResultSubquery(aids: List[Attachment.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.attachment.id.in(aids),
      order = OrderSelection[Attachment.Id](AttachmentType / "id"),
      limit = limit,
      collectionField = "attachments",
      child
    )

  def allocationResultSubquery(pid: Program.Id, child: Query) =
    ResultMapping.mutationResult(child, ResultMapping.MaxLimit, "allocations") { q =>
      FilterOrderByOffsetLimit(
        pred   = Predicates.allocation.id.eql(pid).some,
        oss    = List(OrderSelection[ScienceBand](AllocationType / "scienceBand"), OrderSelection[TimeAccountingCategory](AllocationType / "category")).some,
        offset = None,
        limit  = None,
        child  = q
      )
    }

  def observationResultSubquery(oids: List[Observation.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.observation.id.in(oids),
      order = OrderSelection[Observation.Id](ObservationType / "id"),
      limit = limit,
      collectionField = "observations",
      child
    )

  def programNoteResultSubquery(nids: List[ProgramNote.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate       = Predicates.programNote.id.in(nids),
      order           = OrderSelection[ProgramNote.Id](ProgramNoteType / "id"),
      limit           = limit,
      collectionField = "programNotes",
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

  def programUsersResultSubquery(ids: List[ProgramUser.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    val pred   = Predicates.programUser.id.in(ids)
    val order  = List(
      OrderSelection[Program.Id](ProgramUserType / "program" / "id"),
      OrderSelection[User.Id](ProgramUserType / "user" / "id")
    )
    val limitʹ = limit.foldLeft(ResultMapping.MaxLimit)(_ min _.value) + 1

    ResultMapping.mutationResult(child, limitʹ, "programUsers") { q =>
      FilterOrderByOffsetLimit(
        pred   = pred.some,
        oss    = order.some,
        offset = None,
        limit  = limitʹ.some,
        child  = q
      )
    }

  // Field definitions

  private lazy val AddConditionsEntry: MutationField =
    MutationField("addConditionsEntry", ConditionsEntryInput.Binding): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          chronicleService.addConditionsEntry(input).nestMap: id =>
            Filter(Predicates.addConditionsEntyResult.conditionsEntry.id.eql(id), child)

  private lazy val AddProgramUser: MutationField =
    MutationField("addProgramUser", AddProgramUserInput.Binding): (input, child) =>
      services.useTransactionally:
        programUserService.addProgramUser(input).map: m =>
          m.map: pui =>
            Unique(Filter(Predicates.programUser.id.eql(pui), child))

  private lazy val AddTimeChargeCorrection: MutationField =
    MutationField("addTimeChargeCorrection", AddTimeChargeCorrectionInput.Binding): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          timeAccountingService.addCorrection(input.visitId, input.correction).as:
            Result(
              Filter(Predicates.addTimeChargeCorrectionResult.timeChargeInvoice.id.eql(input.visitId), child)
            )

  private lazy val ChangeProgramUserRole: MutationField =
    MutationField("changeProgramUserRole", ChangeProgramUserRoleInput.Binding): (input, child) =>
      services.useTransactionally:
        programUserService.changeProgramUserRole(input).map: m =>
          m.map: pui =>
            Unique(Filter(Predicates.programUser.id.eql(pui), child))

  private lazy val CloneGroup: MutationField =
    MutationField("cloneGroup", CloneGroupInput.Binding): (input, child) =>
      services.useTransactionally:
        groupService.cloneGroup(input).nestMap: id =>
          Filter(
            And(
              Predicates.cloneGroupResult.originalGroup.id.eql(input.groupId),
              Predicates.cloneGroupResult.newGroup.id.eql(id),
            ), child)

  private lazy val CloneObservation: MutationField =
    MutationField("cloneObservation", CloneObservationInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForClone(input).flatMap: res =>
          res.flatTraverse: checked =>
            if checked.isEmpty then
              OdbError.NotAuthorized(user.id).asFailureF
            else
              observationService.cloneObservation(checked).nestMap: ids =>
                Filter(And(
                  Predicates.cloneObservationResult.originalObservation.id.eql(ids.originalId),
                  Predicates.cloneObservationResult.newObservation.id.eql(ids.cloneId)
                ), child)

  private lazy val ResetAcquisition: MutationField =
    MutationField("resetAcquisition", ResetAcquisitionInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: res =>
          res.flatTraverse: checked =>
            if checked.isEmpty then
              OdbError.NotAuthorized(user.id).asFailureF
            else
              observationService.resetAcquisition(checked).nestMap: oid =>
                Filter(Predicates.resetAcquisitionResult.observation.id.eql(oid), child)

  private lazy val CloneTarget: MutationField =
    MutationField("cloneTarget", CloneTargetInput.Binding): (input, child) =>
      services
        .useNonTransactionally(selectForUpdate(input))
        .flatMap: res =>
          res.flatTraverse: checked =>
            services.useTransactionally:
              targetService.cloneTarget(checked).nestMap: (oldTargetId, newTargetId) =>
                Filter(And(
                  Predicates.cloneTargetResult.originalTarget.id.eql(oldTargetId),
                  Predicates.cloneTargetResult.newTarget.id.eql(newTargetId)
                ), child)

  private lazy val CreateCallForProposals: MutationField =
    MutationField("createCallForProposals", CreateCallForProposalsInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: res =>
          res.flatTraverse: checked =>
            callForProposalsService.createCallForProposals(checked).nestMap: gid =>
              Unique(Filter(Predicates.callForProposals.id.eql(gid), child))

  private lazy val CreateConfigurationRequest: MutationField =
    MutationField("createConfigurationRequest", CreateConfigurationRequestInput.Binding) { (input, child) =>
      services.useTransactionally {
        requirePiAccess {
          configurationService.canonicalizeRequest(input).nestMap { req =>
            Unique(Filter(Predicates.configurationRequest.id.eql(req.id), child))
          }
        }
      }
    }

  private lazy val CreateGroup: MutationField =
    MutationField("createGroup", CreateGroupInput.Binding): (input, child) =>
      services.useTransactionally:
        groupService.createGroup(input).nestMap: gid =>
            Unique(Filter(Predicates.group.id.eql(gid), child))

  private lazy val CreateObservation: MutationField =
    MutationField("createObservation", CreateObservationInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: r =>
          r.flatTraverse:
            case AccessControl.Checked.Empty =>
              OdbError.NotAuthorized(user.id).asFailureF
            case other =>
              observationService.createObservation(other).nestMap: oid =>
                Unique(Filter(Predicates.observation.id.eql(oid), child))

  private lazy val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      services.useTransactionally {
        selectForUpdate(input).flatMap: r =>
          r.flatTraverse: checked =>
            programService.insertProgram(checked).nestMap: id =>
              Unique(Filter(Predicates.program.id.eql(id), child))
      }
    }

  private lazy val CreateProgramNote =
    MutationField("createProgramNote", CreateProgramNoteInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: r =>
          r.flatTraverse:
            case AccessControl.Checked.Empty =>
              OdbError.NotAuthorized(user.id).asFailureF
            case other =>
              programNoteService.createNote(other).nestMap: nid =>
                Unique(Filter(Predicates.programNote.id.eql(nid), child))

  private lazy val CreateProposal =
    MutationField("createProposal", CreateProposalInput.Binding): (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          proposalService.createProposal(input).nestMap: pid =>
            Unique(Filter(Predicates.createProposalResult.programId.eql(pid), child))

  private lazy val CreateTarget =
    MutationField("createTarget", CreateTargetInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: res =>
          res.flatTraverse: checked =>
            targetService.createTarget(checked).nestMap: tid =>
              Unique(Filter(Predicates.target.id.eql(tid), child))

  private lazy val CreateUserInvitation =
    MutationField("createUserInvitation", CreateUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService.createUserInvitation(input).map: rInv =>
          rInv.map: inv =>
            Environment(
              Env("inv" -> inv),
              Unique(Filter(Predicates.userInvitation.id.eql(inv.id), child))
            )

  private lazy val DeleteProgramUser =
    MutationField.json("deleteProgramUser", DeleteProgramUserInput.Binding): input =>
      services.useTransactionally:
        programUserService.deleteProgramUser(input.programUserId).nestMap: b =>
            Json.obj("result" -> b.asJson)

  private lazy val DeleteProposal =
    MutationField.json("deleteProposal", DeleteProposalInput.Binding): input =>
      services.useTransactionally:
        requireStaffAccess:
          proposalService.deleteProposal(input).nestMap: b =>
            Json.obj("result" -> b.asJson)

  private lazy val LinkUser =
    MutationField("linkUser", LinkUserInput.Binding): (input, child) =>
      services.useTransactionally:
        programUserService.linkUser(input).nestMap: _ =>
          Unique(Filter(Predicates.linkUserResult.id.eql(input.programUserId), child))

  private lazy val UnlinkUser =
    MutationField.json("unlinkUser", UnlinkUserInput.Binding): input =>
      services.useTransactionally:
        requirePiAccess:
          programUserService.unlinkUser(input.programUserId).nestMap: o =>
            Json.obj("result" -> o.isDefined.asJson)

  private def recordDatasetResponseToResult(
    child:        Query,
    predicates:   DatasetPredicates
  ): Result[Dataset.Id] => Result[Query] = r =>
      r.map: did =>
        Unique(Filter(predicates.id.eql(did), child))

  private lazy val RecordDataset: MutationField =
    MutationField("recordDataset", RecordDatasetInput.Binding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          datasetService
            .insertDataset(input)
            .map(recordDatasetResponseToResult(child, Predicates.recordDatasetResult.dataset))

  private def addEvent[I: ClassTag: TypeName](
    fieldName: String,
    matcher:   Matcher[I],
    pred:      ExecutionEventPredicates
  )(
    insert:    Services.ServiceAccess ?=> I => (Transaction[F], Services[F]) ?=> F[Result[ExecutionEvent.Id]]
  ): MutationField =
    MutationField(fieldName, matcher): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          insert(input).nestMap: eid =>
            Unique(Filter(pred.id.eql(eid), child))

  private lazy val AddAtomEvent: MutationField =
    addEvent("addAtomEvent", AddAtomEventInput.Binding, Predicates.atomEvent) { input =>
      executionEventService.insertAtomEvent(input)
    }

  private lazy val AddDatasetEvent: MutationField =
    addEvent("addDatasetEvent", AddDatasetEventInput.Binding, Predicates.datasetEvent) { input =>
      executionEventService.insertDatasetEvent(input)
    }

  private lazy val AddSequenceEvent: MutationField =
    addEvent("addSequenceEvent", AddSequenceEventInput.Binding, Predicates.sequenceEvent) { input =>
      executionEventService.insertSequenceEvent(input)
    }

  private lazy val AddSlewEvent: MutationField =
    addEvent("addSlewEvent", AddSlewEventInput.Binding, Predicates.slewEvent) { input =>
      executionEventService.insertSlewEvent(input)
    }

  private lazy val AddStepEvent: MutationField =
    addEvent("addStepEvent", AddStepEventInput.Binding, Predicates.stepEvent) { input =>
      executionEventService.insertStepEvent(input)
    }

  /*
  private def recordAtom(
    response:  F[Result[Atom.Id]],
    predicate: LeafPredicates[Atom.Id],
    child:     Query
  ): F[Result[Query]] =
    response.nestMap: aid =>
      Unique(Filter(predicate.eql(aid), child))

  private lazy val RecordAtom: MutationField =
    MutationField("recordAtom", RecordAtomInput.Binding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordAtom(
            sequenceService.insertAtomRecord(input.visitId, input.instrument, input.sequenceType, input.generatedId, input.idempotencyKey),
            Predicates.atomRecord.id,
            child
          )

  private def recordStep(
    action:    F[Result[Step.Id]],
    predicate: LeafPredicates[Step.Id],
    child:     Query
  ): F[Result[Query]] =
    action.nestMap: sid =>
      Unique(Filter(predicate.eql(sid), child))

  private lazy val RecordFlamingos2Step: MutationField =
    MutationField("recordFlamingos2Step", RecordStepInput.Flamingos2Binding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordStep(
            sequenceService.insertFlamingos2StepRecord(input.atomId, input.instrument, input.stepConfig, input.telescopeConfig, input.observeClass, input.generatedId, input.idempotencyKey, timeEstimateCalculator.flamingos2),
            Predicates.flamingos2Step.id,
            child
          )

  private lazy val RecordGmosNorthStep: MutationField =
    MutationField("recordGmosNorthStep", RecordStepInput.GmosNorthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordStep(
            sequenceService.insertGmosNorthStepRecord(input.atomId, input.instrument, input.stepConfig, input.telescopeConfig, input.observeClass, input.generatedId, input.idempotencyKey, timeEstimateCalculator.gmosNorth),
            Predicates.gmosNorthStep.id,
            child
          )

  private lazy val RecordGmosSouthStep: MutationField =
    MutationField("recordGmosSouthStep", RecordStepInput.GmosSouthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordStep(
            sequenceService.insertGmosSouthStepRecord(input.atomId, input.instrument, input.stepConfig, input.telescopeConfig, input.observeClass, input.generatedId, input.idempotencyKey, timeEstimateCalculator.gmosSouth),
            Predicates.gmosSouthStep.id,
            child
          )
  */

  private def recordVisit(
    response:  F[Result[Visit.Id]],
    predicate: LeafPredicates[Visit.Id],
    child:     Query
  ): F[Result[Query]] =
    ResultT(response).map(vid => Unique(Filter(predicate.eql(vid), child))).value


  private lazy val RecordFlamingos2Visit: MutationField =
    MutationField("recordFlamingos2Visit", RecordVisitInput.Flamingos2Binding): (input, child) =>
      services.useNonTransactionally:
        requireServiceAccess:
          recordVisit(
            visitService.recordFlamingos2(input),
            Predicates.visit.id,
            child
          )

  private lazy val RecordGmosNorthVisit: MutationField =
    MutationField("recordGmosNorthVisit", RecordVisitInput.GmosNorthBinding): (input, child) =>
      services.useNonTransactionally:
        requireServiceAccess:
          recordVisit(
            visitService.recordGmosNorth(input),
            Predicates.visit.id,
            child
          )

  private lazy val RecordGmosSouthVisit: MutationField =
    MutationField("recordGmosSouthVisit", RecordVisitInput.GmosSouthBinding): (input, child) =>
      services.useNonTransactionally:
        requireServiceAccess:
          recordVisit(
            visitService.recordGmosSouth(input),
            Predicates.visit.id,
            child
          )

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

  private lazy val SetAllocations =
    MutationField("setAllocations", SetAllocationsInput.Binding): (input, child) =>
      services.useTransactionally:
        (for
          c <- ResultT(selectForUpdate(input))
          p <- ResultT(allocationService.setAllocations(c))
          q <- ResultT.fromResult(allocationResultSubquery(p, child))
        yield q).value

  private lazy val SetGuideTargetName =
    MutationField("setGuideTargetName", SetGuideTargetNameInput.Binding): (input, child) =>
      services.useNonTransactionally:
        selectForUpdate(input, false /* ignore cals */).flatMap: r =>
          r match
            // This shortcuts setGuideTargetName if we know it has to fail
            case r @ Warning(problems, AccessControl.Checked.Empty) => Failure(problems).pure[F]
            case other =>
              other.flatTraverse: checked =>
                guideService
                  .setGuideTargetName(checked)
                  .nestMap: oid =>
                    Unique(Filter(Predicates.setGuideTargetNameResult.observationId.eql(oid), child))

  private lazy val SetObservationWorkflowState =
    MutationField.encodable("setObservationWorkflowState", SetObservationWorkflowStateInput.Binding): input =>
      services.useNonTransactionally:
        selectForUpdate(input).flatMap: res =>
          res.flatTraverse(observationWorkflowService.setWorkflowState)

  private lazy val SetProgramReference =
    MutationField("setProgramReference", SetProgramReferenceInput.Binding): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: r =>
          r.flatTraverse: checked =>
            programService.setProgramReference(checked).nestMap: (pid, _) =>
              Unique(Filter(Predicates.setProgramReferenceResult.programId.eql(pid), child))

  private lazy val SetProposalStatus =
    MutationField("setProposalStatus", SetProposalStatusInput.Binding): (input, child) =>
      services.useNonTransactionally:
        requirePiAccess:
          proposalService.setProposalStatus(input, commitHash, itcClient, timeEstimateCalculator).nestMap: pid =>
            Unique(Filter(Predicates.setProposalStatusResult.programId.eql(pid), child))

  private lazy val UpdateAsterisms: MutationField =
    MutationField("updateAsterisms", UpdateAsterismsInput.binding(Path.from(ObservationType))) { (input, child) =>

      def selectObservations(using Services[F], NoTransaction[F]): F[Result[(AccessControl.CheckedWithIds[EditAsterismsPatchInput, Observation.Id], Query)]] =
        selectForUpdate(input, false /* exclude cals */).map { r =>
          r.flatMap { update =>
            observationResultSubquery(update.idsOrEmpty, input.LIMIT, child)
              .tupleLeft(update)
          }
        }

      services
        .useNonTransactionally(selectObservations)
        .flatMap: rTup =>
          rTup.flatTraverse:
            case (approved, query) =>
              services
                .useTransactionally:
                  asterismService.updateAsterism(approved)
                    .flatMap: rUnit =>
                      transaction
                        .rollback
                        .unlessA(rUnit.hasValue)
                        .as(rUnit.as(query))

    }

  private lazy val UpdateCallsForProposals: MutationField =
    MutationField("updateCallsForProposals", UpdateCallsForProposalsInput.binding(Path.from(CallForProposalsType))): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: res =>
          res.flatTraverse: checked =>
            callForProposalsService
              .updateCallsForProposals(checked)
              .map(_.flatMap(callForProposalsResultSubquery(_, input.LIMIT, child)))

  private lazy val UpdateConfigurationRequests: MutationField =
    MutationField("updateConfigurationRequests", UpdateConfigurationRequestsInput.binding(Path.from(ConfigurationRequestType))): (input, child) =>
      services.useTransactionally:
        idSelectFromPredicate(
          ConfigurationRequestType,
            and(List(
            Predicates.configurationRequest.program.isWritableBy(user),
            input.WHERE.getOrElse(True)
          ))
        ).flatTraverse: which =>
          configurationService
            .updateRequests(input.SET, which)
            .map(_.flatMap(configurationRequestResultSubquery(_, input.LIMIT, child)))

  private lazy val UpdateDatasets: MutationField =
    MutationField("updateDatasets", UpdateDatasetsInput.binding(Path.from(DatasetType))): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          idSelectFromPredicate(
            DatasetType,
            and(List(
              Predicates.dataset.observation.program.isWritableBy(user),
              input.WHERE.getOrElse(True)
            ))
          ).flatTraverse: which =>
            datasetService
              .updateDatasets(input.SET, which)
              .map(datasetResultSubquery(_, input.LIMIT, child))

  private lazy val UpdateAttachments =
    MutationField("updateAttachments", UpdateAttachmentsInput.binding(Path.from(AttachmentType))): (input, child) =>
      services.useTransactionally:
        selectForUpdate(input).flatMap: r =>
          r.flatTraverse: checked =>
            attachmentMetadataService
              .updateAttachments(checked)
              .map: r =>
                r.flatMap(attachmentResultSubquery(_, input.LIMIT, child))

  private lazy val UpdateProgramNotes: MutationField =
    MutationField("updateProgramNotes", UpdateProgramNotesInput.binding(Path.from(ProgramNoteType))): (input, child) =>
      services.useTransactionally:
        ResultT(selectForUpdate(input)).flatMap: checked =>
          ResultT:
            programNoteService
              .updateNotes(checked).map: nids =>
                programNoteResultSubquery(nids, input.LIMIT, child)
        .value

  private lazy val UpdateObservations: MutationField =
    MutationField("updateObservations", UpdateObservationsInput.binding(Path.from(ObservationType))) { (input, child) =>

      def updateObservations(checked: AccessControl.Checked[ObservationPropertiesInput.Edit])(using Services[F], Transaction[F]): ResultT[F, (Map[Program.Id, List[Observation.Id]], Query)] =
        ResultT:
          observationService
            .updateObservations(checked)
            .map { r =>
              r.flatMap { m =>
                val oids = m.values.foldLeft(List.empty[Observation.Id])(_ ++ _)
                observationResultSubquery(oids, input.LIMIT, child).tupleLeft(m)
              }
            }

      def setAsterisms(m: Map[Program.Id, List[Observation.Id]])(using Services[F], Transaction[F], SuperUserAccess): ResultT[F, Unit] =
        ResultT:
          // The "obvious" implementation with `traverse` doesn't work here because
          // ResultT isn't a Monad and thus doesn't short-circuit. Doing an explicit
          // fold with the [non-monadic] short-circuiting `flatMap` fixes it.
          m.toList.foldRight(ResultT(Result.unit.pure[F])) { case ((pid, oids), accum) =>
            ResultT(NonEmptyList.fromList(oids).fold(Result.unit.pure[F]) { os =>
              asterismService.setAsterism(pid, os, input.asterism)
            }).flatMap(_ => accum)
          }.value

      // Put it all together
      services
        .useNonTransactionally(selectForUpdate(input, false /* ignore calibrations */)) // this performs all the access control checks
        .flatMap: approval =>
          services.useTransactionally:
            approval.flatTraverse: edit =>
                updateObservations(edit)
                  .flatMap:
                    case (map, query) =>
                      Services.asSuperUser:
                        setAsterisms(map)
                          .as(query)
                  .value
                  .flatTap: q =>
                    transaction.rollback.unlessA(q.hasValue)

    }

  private lazy val UpdateObservationsTimes: MutationField =
    MutationField("updateObservationsTimes", UpdateObservationsTimesInput.binding(Path.from(ObservationType))): (input, child) =>
      services
        .useNonTransactionally(selectForUpdate(input, true)) // include cals
        .flatMap: result =>
          result.flatTraverse: approved =>
            services.useTransactionally:
              observationService
                .updateObservationsTimes(approved)
                .map: r =>
                  r.flatMap: m =>
                    observationResultSubquery(m.values.flatten.toList, input.LIMIT, child)

  private lazy val UpdateProgramUsers =
    MutationField("updateProgramUsers", UpdateProgramUsersInput.binding(Path.from(ProgramUserType))): (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          idSelectFromPredicate(
            ProgramUserType,
            and(List(
              Predicates.programUser.program.isWritableBy(user),
              input.WHERE.getOrElse(True)
            ))
          ).flatTraverse: which =>
            programUserService
              .updateProperties(input.SET, which)
              .map(
                _.flatMap(programUsersResultSubquery(_, input.LIMIT, child))
              )

  private lazy val UpdatePrograms =
    MutationField("updatePrograms", UpdateProgramsInput.binding(Path.from(ProgramType))): (input, child) =>
      services.useTransactionally:
        (for
          checked <- ResultT(selectForUpdate(input))
          pids    <- ResultT(programService.updatePrograms(checked))
          query   <- ResultT.fromResult(programResultSubquery(pids, input.LIMIT, child))
        yield query).value

  private lazy val UpdateProposal =
    MutationField("updateProposal", UpdateProposalInput.Binding) { (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          proposalService.updateProposal(input).nestMap: pid =>
            Unique(Filter(Predicates.updateProposalResult.programId.eql(pid), child))
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
    MutationField("updateTargets", UpdateTargetsInput.binding(Path.from(TargetType))): (input, child) =>
      services
        .useNonTransactionally(selectForUpdate(input))
        .flatMap: res =>
          res.flatTraverse: checked =>
            ResultT(services.useTransactionally(targetService.updateTargets(checked)))
              .flatMap: selected =>
                ResultT(targetResultSubquery(selected, input.LIMIT, child).pure[F])
              .value

  def groupResultSubquery(pids: List[Group.Id], limit: Option[NonNegInt], child: Query): Result[Query] =
    mutationResultSubquery(
      predicate = Predicates.group.id.in(pids),
      order = OrderSelection[Group.Id](GroupType / "id"),
      limit = limit,
      collectionField = "groups",
      child
    )

  private lazy val UpdateGroups =
    MutationField("updateGroups", UpdateGroupsInput.binding(Path.from(GroupType))): (input, child) =>
      services
        .useTransactionally:
          idSelectFromPredicate(
            GroupType,
            and(List(
              // TODO: Predicates.group.program.isWritableBy(user),
              input.WHERE.getOrElse(True)
            ))
          ).flatTraverse: which =>
            groupService.updateGroups(input.SET, which).map: r =>
              r.flatMap: selected =>
                groupResultSubquery(selected, input.LIMIT, child)
        .recover: // need to recover here due to deferred constraints; nothing bad happens until we commit
          case SqlState.RaiseException(ex) =>
            OdbError.InconsistentGroupError(Some(ex.message)).asFailure

}
