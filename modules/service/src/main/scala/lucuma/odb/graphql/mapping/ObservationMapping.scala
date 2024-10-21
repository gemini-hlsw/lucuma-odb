// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.functor.*
import cats.syntax.option.*
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services

import table.ObsAttachmentAssignmentTable
import table.ObsAttachmentTable
import table.ObservationReferenceView
import table.ProgramTable
import Services.Syntax.*

trait ObservationMapping[F[_]]
  extends ObservationEffectHandler[F]
     with ProgramTable[F]
     with TimingWindowView[F]
     with ObsAttachmentTable[F]
     with ObsAttachmentAssignmentTable[F]
     with ObservationReferenceView[F] {

  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(ObservationType)(
      SqlField("id", ObservationView.Id, key = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      SqlField("existence", ObservationView.Existence),
      SqlObject("reference", Join(ObservationView.Id, ObservationReferenceView.Id)),
      SqlField("index", ObservationView.ObservationIndex),
      SqlField("title", ObservationView.Title),
      SqlField("subtitle", ObservationView.Subtitle),
      SqlField("status", ObservationView.Status),
      SqlField("activeStatus", ObservationView.ActiveStatus),
      SqlField("scienceBand", ObservationView.ScienceBand),
      SqlField("observationTime", ObservationView.ObservationTime),
      SqlObject("observationDuration"),
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
      EffectField("validations", validationsQueryHandler, List("id", "programId")),
      SqlObject("execution"),
      SqlField("groupId", ObservationView.GroupId),
      SqlField("groupIndex", ObservationView.GroupIndex),
      SqlField("calibrationRole", ObservationView.CalibrationRole),
      SqlField("observerNotes", ObservationView.ObserverNotes),
      SqlObject("configuration"),
      EffectField("configurationRequests", configurationRequestsQueryHandler, List("id", "programId")),
      SqlField("forReview", ObservationView.ForReview),
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

    import ItcService.Error.ObservationDefinitionError

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[ItcService.AsterismResults]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          itcService(itcClient)
            .lookup(pid, oid)
            .map {
              case Left(e@ObservationDefinitionError(_)) => OdbError.InvalidObservation(oid, e.format.some).asFailure
              case Left(e)                               => OdbError.ItcError(e.format.some).asFailure
              case Right(s)                              => s.success
            }
        }

    effectHandler(readEnv, calculate)
  }

  def validationsQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[List[ObservationValidation]]] =
      (pid, oid, _) =>
        services.useTransactionally {
          observationService
            .observationValidations(pid, oid, itcClient)
            .map(_.success)
        }

    effectHandler(readEnv, calculate)
  }

  lazy val configurationRequestsQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[List[ConfigurationRequest]]] =
      (_, oid, _) =>
        services.useTransactionally {
          configurationService
            .selectRequests(oid)
        }

    effectHandler(readEnv, calculate)
  }

}
