// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.functor.*
import grackle.Env
import grackle.Query
import grackle.Query._
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services

import table.ObsAttachmentAssignmentTable
import table.ObsAttachmentTable
import table.ProgramTable
import Services.Syntax.*
import lucuma.odb.service.withDetail
import lucuma.odb.service.asFailure

trait ObservationMapping[F[_]]
  extends ObservationEffectHandler[F]
     with ProgramTable[F]
     with TimingWindowView[F]
     with ObsAttachmentTable[F]
     with ObsAttachmentAssignmentTable[F] {

  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservationType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true),
        SqlField("programId", ObservationView.ProgramId, hidden = true),
        SqlField("existence", ObservationView.Existence, hidden = true),
        SqlField("title", ObservationView.Title),
        SqlField("subtitle", ObservationView.Subtitle),
        SqlField("status", ObservationView.Status),
        SqlField("activeStatus", ObservationView.ActiveStatus),
        SqlField("visualizationTime", ObservationView.VisualizationTime),
        SqlObject("posAngleConstraint"),
        SqlObject("targetEnvironment"),
        SqlObject("constraintSet"),
        SqlObject("timingWindows", Join(ObservationView.Id, TimingWindowView.ObservationId)),
        SqlObject("obsAttachments",
          Join(ObservationView.Id, ObsAttachmentAssignmentTable.ObservationId),
          Join(ObsAttachmentAssignmentTable.ObsAttachmentId, ObsAttachmentTable.Id)),
        SqlObject("scienceRequirements"),
        SqlObject("observingMode"),
        SqlField("instrument", ObservationView.Instrument),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id)),
        EffectField("itc", itcQueryHandler, List("id", "programId")),
        SqlObject("execution"),
        SqlField("groupId", ObservationView.GroupId),
        SqlField("groupIndex", ObservationView.GroupIndex),
      )
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

    case (ObservationType, "obsAttachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"))), child)
      }

    case (ObservationType, "itc", List(BooleanBinding.Option("useCache", rUseCache))) =>
      Elab.transformChild { child =>
          rUseCache.as(child)
      }

  }

  def itcQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[ItcService.AsterismResult]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          itcService(itcClient)
           .lookup(pid, oid)
           .map {
             case Left(e)  => error.itcError.withDetail(e.format).asFailure
             case Right(s) => s.success
           }
        }

    effectHandler(readEnv, calculate)
  }

}
