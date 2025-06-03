// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all.*
import grackle.Context
import grackle.Cursor
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.syntax.*
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.SuperUserAccess
import skunk.Transaction

import table.AttachmentTable
import table.ObsAttachmentAssignmentTable
import table.ObscalcTable
import table.ObservationReferenceView
import table.ProgramTable
import Services.Syntax.*

trait ObservationMapping[F[_]]
  extends ObservationEffectHandler[F]
     with ProgramTable[F]
     with TimingWindowView[F]
     with AttachmentTable[F]
     with ObsAttachmentAssignmentTable[F]
     with ObscalcTable[F]
     with ObservationReferenceView[F] {

  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(ObservationType)(
      SqlField("id", ObservationView.Id, key = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      SqlField("existence", ObservationView.Existence),
      SqlObject("reference", Join(ObservationView.Id, ObservationReferenceView.Id)),
      SqlField("index", ObservationView.ObservationIndex),
      SqlField("title", ObservationView.Title),
      SqlField("subtitle", ObservationView.Subtitle),
      SqlField("scienceBand", ObservationView.ScienceBand),
      SqlField("observationTime", ObservationView.ObservationTime),
      SqlObject("observationDuration"),
      SqlObject("posAngleConstraint"),
      SqlObject("targetEnvironment"),
      SqlObject("constraintSet"),
      SqlObject("timingWindows", Join(ObservationView.Id, TimingWindowView.ObservationId)),
      SqlObject("attachments",
        Join(ObservationView.Id, ObsAttachmentAssignmentTable.ObservationId),
        Join(ObsAttachmentAssignmentTable.AttachmentId, AttachmentTable.Id)),
      SqlObject("scienceRequirements"),
      SqlObject("observingMode"),
      SqlField("instrument", ObservationView.Instrument),
      SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id)),
      EffectField("itc", itcQueryHandler, List("id", "programId")),
      SqlObject("execution"),
      SqlField("groupId", ObservationView.GroupId),
      SqlField("groupIndex", ObservationView.GroupIndex),
      SqlField("calibrationRole", ObservationView.CalibrationRole),
      SqlField("observerNotes", ObservationView.ObserverNotes),
      SqlObject("configuration"),
      EffectField("configurationRequests", configurationRequestsQueryHandler, List("id", "programId")),
      EffectField("workflow", workflowQueryHandler, List("id", "programId")),
      SqlObject("calculatedWorkflow", Join(ObservationView.Id, ObscalcTable.ObservationId))
//      EffectField("calculatedWorkflow", calculatedWorkflowQueryHandler, List("id", "programId")),
    )

  lazy val ObservationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (ObservationType, "timingWindows", Nil) =>
      Elab.transformChild { child =>
        FilterOrderByOffsetLimit(
          pred = None,
          oss = Some(List(
            OrderSelection[Long](TimingWindowType / "id", true, true)
          )),
          offset = None,
          limit = None,
          child
        )
      }

    case (ObservationType, "attachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Attachment.Id](AttachmentType / "id"))), child)
      }

    case (ObservationType, "itc", List(BooleanBinding.Option("useCache", rUseCache))) =>
      Elab.transformChild { child =>
          rUseCache.as(child)
      }

  }

  def itcQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[ItcService.AsterismResults]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          itcService(itcClient)
            .lookup(pid, oid)
            .map(_.bimap(_.asFailure, _.success).merge)
        }

    effectHandler(readEnv, calculate)
  }

  lazy val configurationRequestsQueryHandler: EffectHandler[F] = { pairs =>

    // Here's the collection of stuff we need to deal with: a pid+oid pair, the parent
    // cursor, and the child context.
    val sequence: ResultT[F, List[((Program.Id, Observation.Id), Cursor, Context)]] =
      ResultT.fromResult:
        pairs.traverse: (query, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
            c <- Query.childContext(cursor.context, query)
          } yield ((p, o), cursor, c)

    // Pass the pid+oid pairs to configurationService.selectRequests to get the
    // applicable configuration requests for each pair, then use this information
    // to construct our list of outgoing cursors.
    def query(using Services[F], Transaction[F]): ResultT[F, List[Cursor]] =
      sequence.flatMap: pairs =>
        ResultT(configurationService.selectRequests(pairs.map(_._1))).map: reqs =>
          pairs.map: (key, cursor, childContext) =>
            CirceCursor(childContext, reqs(key).asJson, Some(cursor), cursor.fullEnv)

    // Do it!
    services.useTransactionally:
      query.value

  }

  trait WorkflowEffectHandler extends EffectHandler[F]:

    // Here's the collection of stuff we need to deal with: an oid, the parent
    // cursor, and the child context.
    protected def sequence(pairs: List[(Query, Cursor)]): ResultT[F, List[(Observation.Id, Cursor, Context)]] =
      ResultT.fromResult:
        pairs.traverse: (query, cursor) =>
          for
            o <- cursor.fieldAs[Observation.Id]("id")
            c <- Query.childContext(cursor.context, query)
          yield (o, cursor, c)

  lazy val workflowQueryHandler: EffectHandler[F] =
    new WorkflowEffectHandler:

      def runEffects(pairs: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        // Pass the oids to observationWorkflowService.getWorkflows to get the
        // applicable workflows for each, then use this information to construct
        // our list of outgoing cursors.
        def query(using Services[F], SuperUserAccess): ResultT[F, List[Cursor]] =
          sequence(pairs).flatMap: tuples =>
            ResultT(observationWorkflowService.getWorkflows(tuples.map(_._1), commitHash, itcClient, timeEstimateCalculator)).map: reqs =>
              tuples.map: (key, cursor, childContext) =>
                CirceCursor(childContext, reqs(key).asJson, Some(cursor), cursor.fullEnv)

        // Do it!
        services.use: s =>
          Services.asSuperUser:
            query(using s).value
/*
  lazy val calculatedWorkflowQueryHandler: EffectHandler[F] =
    new WorkflowEffectHandler:

      def runEffects(pairs: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        import lucuma.odb.json.calculatedValue.given

        // Pass the oids to observationWorkflowService.getCalculatedWorkflows to get the
        // applicable workflows for each, then use this information to construct
        // our list of outgoing cursors.
        def query(using Services[F], Transaction[F]): ResultT[F, List[Cursor]] =
          sequence(pairs).flatMap: tuples =>
            ResultT(observationWorkflowService.getCalculatedWorkflows(tuples.map(_._1), commitHash, itcClient, timeEstimateCalculator)).map: reqs =>
              tuples.map: (key, cursor, childContext) =>
                CirceCursor(childContext, reqs(key).asJson, Some(cursor), cursor.fullEnv)

        // Do it!
        services.useTransactionally:
          query.value
*/
}
