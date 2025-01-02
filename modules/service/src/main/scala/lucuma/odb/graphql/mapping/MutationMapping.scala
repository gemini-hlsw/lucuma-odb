// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Functor
import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.cats.*
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
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Attachment
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.predicate.DatasetPredicates
import lucuma.odb.graphql.predicate.ExecutionEventPredicates
import lucuma.odb.graphql.predicate.LeafPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import org.http4s.client.Client
import org.tpolecat.typename.TypeName
import skunk.AppliedFragment
import skunk.SqlState
import skunk.Transaction

import scala.reflect.ClassTag

trait MutationMapping[F[_]] extends Predicates[F] {

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
      CloneGroup,
      CloneObservation,
      CloneTarget,
      CreateCallForProposals,
      CreateConfigurationRequest,
      CreateGroup,
      CreateObservation,
      CreateProgram,
      CreateProposal,
      CreateTarget,
      CreateUserInvitation,
      DeleteProgramUser,
      DeleteProposal,
      LinkUser,
      RecordAtom,
      RecordDataset,
      RecordGmosNorthStep,
      RecordGmosNorthVisit,
      RecordGmosSouthStep,
      RecordGmosSouthVisit,
      RedeemUserInvitation,
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
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  val httpClient: Client[F]
  val itcClient: ItcClient[F]
  def emailConfig: Config.Email
  val commitHash: CommitHash

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

  def programResultSubquery(pids: List[Program.Id], limit: Option[NonNegInt], child: Query) =
    mutationResultSubquery(
      predicate = Predicates.program.id.in(pids),
      order = OrderSelection[Program.Id](ProgramType / "id"),
      limit = limit,
      collectionField = "programs",
      child
    )

  def programUsersResultSubquery(ids: List[ProgramUser.Id], limit: Option[NonNegInt], child: Query) =
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

  // We do this a lot
  extension [F[_]: Functor, G[_]: Functor, A](fga: F[G[A]])
    def nestMap[B](fab: A => B): F[G[B]] = fga.map(_.map(fab))
    def nestAs[B](b: B): F[G[B]] = fga.map(_.as(b))

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
        programUserService.addProgramUser(input).map: r =>
          r.map: pui =>
            Unique(Filter(Predicates.programUser.id.eql(pui) , child))

  private lazy val AddTimeChargeCorrection: MutationField =
    MutationField("addTimeChargeCorrection", AddTimeChargeCorrectionInput.Binding): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          timeAccountingService.addCorrection(input.visitId, input.correction).as:
            Result(
              Filter(Predicates.addTimeChargeCorrectionResult.timeChargeInvoice.id.eql(input.visitId), child)
            )

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
        observationService.cloneObservation(input).nestMap: ids =>
          Filter(And(
            Predicates.cloneObservationResult.originalObservation.id.eql(ids.originalId),
            Predicates.cloneObservationResult.newObservation.id.eql(ids.cloneId)
          ), child)

  private lazy val CloneTarget: MutationField =
    MutationField("cloneTarget", CloneTargetInput.Binding): (input, child) =>
      services.useTransactionally:
        targetService.cloneTarget(input).nestMap: (oldTargetId, newTargetId) =>
          Filter(And(
            Predicates.cloneTargetResult.originalTarget.id.eql(oldTargetId),
            Predicates.cloneTargetResult.newTarget.id.eql(newTargetId)
          ), child)

  private lazy val CreateCallForProposals: MutationField =
    MutationField("createCallForProposals", CreateCallForProposalsInput.Binding) { (input, child) =>
      services.useTransactionally {
        requireStaffAccess {
          callForProposalsService.createCallForProposals(input).nestMap { gid =>
            Unique(Filter(Predicates.callForProposals.id.eql(gid), child))
          }
        }
      }
    }

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
        observationService.createObservation(input).nestMap: oid =>
          Unique(Filter(Predicates.observation.id.eql(oid), child))

  private lazy val CreateProgram =
    MutationField("createProgram", CreateProgramInput.Binding) { (input, child) =>
      services.useTransactionally {
        programService.insertProgram(input.SET).nestMap: id =>
          Unique(Filter(Predicates.program.id.eql(id), child))
      }
    }

  private lazy val CreateProposal =
    MutationField("createProposal", CreateProposalInput.Binding): (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          proposalService.createProposal(input).nestMap: pid =>
            Unique(Filter(Predicates.createProposalResult.programId.eql(pid), child))

  private lazy val CreateTarget =
    MutationField("createTarget", CreateTargetInput.Binding): (input, child) =>
      services.useTransactionally:
        targetService.createTarget(input).nestMap: tid =>
          Unique(Filter(Predicates.target.id.eql(tid), child))

  private lazy val CreateUserInvitation =
    MutationField("createUserInvitation", CreateUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService(emailConfig, httpClient).createUserInvitation(input).map: rInv =>
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
    insert:    Services.ServiceAccess ?=> I => (Transaction[F], Services[F]) ?=> F[Result[ExecutionEvent]]
  ): MutationField =
    MutationField(fieldName, matcher): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          insert(input).nestMap: e =>
            Unique(Filter(pred.id.eql(e.id), child))

  private lazy val AddAtomEvent: MutationField =
    addEvent("addAtomEvent", AddAtomEventInput.Binding, Predicates.atomEvent) { input =>
      executionEventService.insertAtomEvent(input.atomId, input.atomStage)
    }

  private lazy val AddDatasetEvent: MutationField =
    addEvent("addDatasetEvent", AddDatasetEventInput.Binding, Predicates.datasetEvent) { input =>
      executionEventService.insertDatasetEvent(input.datasetId, input.datasetStage)
    }

  private lazy val AddSequenceEvent: MutationField =
    addEvent("addSequenceEvent", AddSequenceEventInput.Binding, Predicates.sequenceEvent) { input =>
      executionEventService.insertSequenceEvent(input.visitId, input.command)
    }

  private lazy val AddSlewEvent: MutationField =
    addEvent("addSlewEvent", AddSlewEventInput.Binding, Predicates.slewEvent) { input =>
      executionEventService.insertSlewEvent(input.visitId, input.slewStage)
    }

  private lazy val AddStepEvent: MutationField =
    addEvent("addStepEvent", AddStepEventInput.Binding, Predicates.stepEvent) { input =>
      executionEventService.insertStepEvent(input.stepId, input.stepStage)
    }

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
            sequenceService.insertAtomRecord(input.visitId, input.instrument, input.sequenceType, input.generatedId),
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

  private lazy val RecordGmosNorthStep: MutationField =
    MutationField("recordGmosNorthStep", RecordGmosStepInput.GmosNorthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordStep(
            sequenceService.insertGmosNorthStepRecord(input.atomId, input.instrument, input.stepConfig, input.telescopeConfig, input.observeClass, input.generatedId, timeEstimateCalculator.gmosNorth),
            Predicates.gmosNorthStep.id,
            child
          )

  private lazy val RecordGmosSouthStep: MutationField =
    MutationField("recordGmosSouthStep", RecordGmosStepInput.GmosSouthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordStep(
            sequenceService.insertGmosSouthStepRecord(input.atomId, input.instrument, input.stepConfig, input.telescopeConfig, input.observeClass, input.generatedId, timeEstimateCalculator.gmosSouth),
            Predicates.gmosSouthStep.id,
            child
          )

  private def recordVisit(
    response:  F[Result[Visit.Id]],
    predicate: LeafPredicates[Visit.Id],
    child:     Query
  )(using Services[F], Transaction[F]): F[Result[Query]] =
    ResultT(response).map(vid => Unique(Filter(predicate.eql(vid), child))).value

  private lazy val RecordGmosNorthVisit: MutationField =
    MutationField("recordGmosNorthVisit", RecordGmosVisitInput.GmosNorthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordVisit(
            visitService.insertGmosNorth(input.observationId, input.static),
            Predicates.visit.id,
            child
          )

  private lazy val RecordGmosSouthVisit: MutationField =
    MutationField("recordGmosSouthVisit", RecordGmosVisitInput.GmosSouthBinding): (input, child) =>
      services.useTransactionally:
        requireServiceAccess:
          recordVisit(
            visitService.insertGmosSouth(input.observationId, input.static),
            Predicates.visit.id,
            child
          )

  private lazy val RedeemUserInvitation =
    MutationField("redeemUserInvitation", RedeemUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService(emailConfig, httpClient).redeemUserInvitation(input).map: rId =>
          rId.map: id =>
            Unique(Filter(Predicates.userInvitation.id.eql(id), child))

  private lazy val RevokeUserInvitation =
    MutationField("revokeUserInvitation", RevokeUserInvitationInput.Binding): (input, child) =>
      services.useTransactionally:
        userInvitationService(emailConfig, httpClient).revokeUserInvitation(input).map: rId =>
          rId.map: id =>
            Unique(Filter(Predicates.userInvitation.id.eql(id), child))

  private lazy val SetAllocations =
    MutationField("setAllocations", SetAllocationsInput.Binding): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          allocationService.setAllocations(input).map(_ *>
            allocationResultSubquery(input.programId, child)
          )

  private lazy val SetGuideTargetName = 
    MutationField("setGuideTargetName", SetGuideTargetNameInput.Binding): (input, child) =>
      services.useNonTransactionally:
        guideService(httpClient, itcClient, commitHash, timeEstimateCalculator).setGuideTargetName(input)
          .nestMap: oid =>
            Unique(Filter(Predicates.setGuideTargetNameResult.observationId.eql(oid), child))

  private lazy val SetObservationWorkflowState =
    MutationField.encodable("setObservationWorkflowState", SetObservationWorkflowStateInput.Binding): input =>
      services.useNonTransactionally:
        observationWorkflowService.setWorkflowState(input.observationId, input.state, commitHash, itcClient, timeEstimateCalculator)

  private lazy val SetProgramReference =
    MutationField("setProgramReference", SetProgramReferenceInput.Binding): (input, child) =>
      services.useTransactionally:
        requireStaffAccess:
          programService.setProgramReference(input).nestMap: (pid, _) =>
            Unique(Filter(Predicates.setProgramReferenceResult.programId.eql(pid), child))

  private lazy val SetProposalStatus =
    MutationField("setProposalStatus", SetProposalStatusInput.Binding): (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          proposalService.setProposalStatus(input).nestMap: pid =>
            Unique(Filter(Predicates.setProposalStatusResult.programId.eql(pid), child))

  // An applied fragment that selects all observation ids that satisfy
  // `filterPredicate`
  private def observationIdSelect(
    includeDeleted:      Option[Boolean],
    WHERE:               Option[Predicate],
    includeCalibrations: Boolean
  ): Result[AppliedFragment] = {
    val whereObservation: Predicate =
      and(List(
        Predicates.observation.program.isWritableBy(user),
        Predicates.observation.existence.includeDeleted(includeDeleted.getOrElse(false)),
        if (includeCalibrations) True else Predicates.observation.calibrationRole.isNull(true),
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
          observationIdSelect(input.includeDeleted, input.WHERE, false)

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
            asterismService.updateAsterism(os, add, del)
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

  private lazy val UpdateCallsForProposals: MutationField =
    MutationField("updateCallsForProposals", UpdateCallsForProposalsInput.binding(Path.from(CallForProposalsType))) { (input, child) =>
      services.useTransactionally {
        requireStaffAccess {
          val p = and(List(
            Predicates.callForProposals.existence.includeDeleted(input.includeDeleted.getOrElse(false)),
            input.WHERE.getOrElse(True)
          ))

          val idSelect: Result[AppliedFragment] =
            MappedQuery(
              Filter(p, Select("id", None, Empty)),
              Context(QueryType, List("callsForProposals"), List("callsForProposals"), List(CallForProposalsType))
            ).flatMap(_.fragment)

          idSelect.flatTraverse { which =>
            callForProposalsService
              .updateCallsForProposals(input.SET, which)
              .map(_.flatMap(callForProposalsResultSubquery(_, input.LIMIT, child)))
          }
        }
      }
    }

  private lazy val UpdateConfigurationRequests: MutationField =
    MutationField("updateConfigurationRequests", UpdateConfigurationRequestsInput.binding(Path.from(ConfigurationRequestType))) { (input, child) =>
      services.useTransactionally {

        // Our predicate for selecting requests to update
        val filterPredicate = and(List(
          Predicates.configurationRequest.program.isWritableBy(user),
          input.WHERE.getOrElse(True)
        ))

        val idSelect: Result[AppliedFragment] =
          MappedQuery(Filter(filterPredicate, Select("id", Empty)), Context(QueryType, List("requests"), List("requests"), List(ConfigurationRequestType))).flatMap(_.fragment)

        idSelect.flatTraverse { which =>
          configurationService
            .updateRequests(input.SET, which)
            .map(_.flatMap(configurationRequestResultSubquery(_, input.LIMIT, child)))
        }

      }
    }

  private lazy val UpdateDatasets: MutationField =
    MutationField("updateDatasets", UpdateDatasetsInput.binding(Path.from(DatasetType))) { (input, child) =>
      services.useTransactionally {
        requireStaffAccess {
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
    }

  private lazy val UpdateAttachments =
    MutationField("updateAttachments", UpdateAttachmentsInput.binding(Path.from(AttachmentType))) { (input, child) =>
      services.useTransactionally {
        val filterPredicate = and(List(
          Predicates.attachment.program.isWritableBy(user),
          input.WHERE.getOrElse(True)
        ))

        val idSelect: Result[AppliedFragment] =
          MappedQuery(
            Filter(filterPredicate, Select("id", Empty)),
            Context(QueryType, List("attachments"), List("attachments"), List(AttachmentType))
          ).flatMap(_.fragment)

        idSelect.flatTraverse { which =>
          attachmentMetadataService.updateAttachments(input.SET, which).map(attachmentResultSubquery(_, input.LIMIT, child))
        }
      }
    }

  private lazy val UpdateObservations: MutationField =
    MutationField("updateObservations", UpdateObservationsInput.binding(Path.from(ObservationType))) { (input, child) =>
      services.useTransactionally {

        val idSelect: Result[AppliedFragment] =
          observationIdSelect(input.includeDeleted, input.WHERE, false)

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
          // The "obvious" implementation with `traverse` doesn't work here because
          // ResultT isn't a Monad and thus doesn't short-circuit. Doing an explicit
          // fold with the [non-monadic] short-circuiting `flatMap` fixes it.
          m.toList.foldRight(ResultT(Result.unit.pure[F])) { case ((pid, oids), accum) =>
            ResultT(NonEmptyList.fromList(oids).fold(Result.unit.pure[F]) { os =>
              asterismService.setAsterism(pid, os, input.asterism)
            }).flatMap(_ => accum)
          }.value

        val r = for {
          tup <- ResultT(updateObservations)
          _   <- ResultT(setAsterisms(tup._1))
        } yield tup._2

        r.value.flatTap { q => transaction.rollback.unlessA(q.hasValue) }
      }
    }

  private lazy val UpdateObservationsTimes: MutationField =
    MutationField("updateObservationsTimes", UpdateObservationsTimesInput.binding(Path.from(ObservationType))) { (input, child) =>
      services.useTransactionally {

        val idSelect: Result[AppliedFragment] =
          observationIdSelect(input.includeDeleted, input.WHERE, true)

        val updateObservations: F[Result[(Map[Program.Id, List[Observation.Id]], Query)]] =
          idSelect.flatTraverse { which =>
            observationService
              .updateObservationsTimes(input.SET, which)
              .map { r =>
                r.flatMap { m =>
                  val oids = m.values.foldLeft(List.empty[Observation.Id])(_ ++ _)
                  observationResultSubquery(oids, input.LIMIT, child).tupleLeft(m)
                }
              }
          }

        updateObservations.map(_.map(_._2))
      }
    }

  private lazy val UpdateProgramUsers =
    MutationField("updateProgramUsers", UpdateProgramUsersInput.binding(Path.from(ProgramUserType))): (input, child) =>
      services.useTransactionally:
        requirePiAccess:
          val filterPredicate = and(List(
            Predicates.programUser.program.isWritableBy(user),
            input.WHERE.getOrElse(True)
          ))

          val selection: Result[AppliedFragment] =
            MappedQuery(
              Filter(filterPredicate, Query.Group(List(Select("id", None, Empty)))),
              Context(QueryType, List("programUsers"), List("programUsers"), List(ProgramUserType))
            ).flatMap(_.fragment)

          selection.flatTraverse: which =>
            programUserService
              .updateProperties(input.SET, which)
              .map(
                _.flatMap(programUsersResultSubquery(_, input.LIMIT, child))
              )

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
        idSelect.flatTraverse: which =>
          ResultT(targetService.updateTargets(input.SET, which)).flatMap: selected =>
            ResultT(targetResultSubquery(selected, input.LIMIT, child).pure[F])
          .value

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
        idSelect.flatTraverse: which =>
          groupService.updateGroups(input.SET, which).map: r =>
            r.flatMap: selected =>
              groupResultSubquery(selected, input.LIMIT, child)

      } .recover: // need to recover here due to deferred constraints; nothing bad happens until we commit
        case SqlState.RaiseException(ex) =>
          OdbError.InconsistentGroupError(Some(ex.message)).asFailure

    }

}
