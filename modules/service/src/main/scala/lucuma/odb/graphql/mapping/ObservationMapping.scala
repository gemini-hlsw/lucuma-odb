// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.functor.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services

import table.ObsAttachmentAssignmentTable
import table.ObsAttachmentTable
import table.ProgramTable


trait ObservationMapping[F[_]]
  extends ObservationEffectHandler[F]
     with ProgramTable[F]
     with TimingWindowView[F]
     with ObsAttachmentTable[F]
     with ObsAttachmentAssignmentTable[F] {

  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode

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
      )
    )

  lazy val ObservationElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ObservationType -> {
        case Select("timingWindows", Nil, child) =>
          Result(
            Select("timingWindows", Nil,
              FilterOrderByOffsetLimit(
                pred = None,
                oss = Some(List(
                  OrderSelection[Long](TimingWindowType / "id", true, true)
                )),
                offset = None,
                limit = None,
                child
              )
            )
          )

        case Select("obsAttachments", Nil, child) =>
          Result(
            Select("obsAttachments", Nil,
              OrderBy(OrderSelections(List(OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"))), child)
            )
          )

        case Select("itc", List(BooleanBinding.Option("useCache", rUseCache)), child) =>
          rUseCache.map { _ => Select("itc", Nil, child) }

      }
    )

  def itcQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[ItcService.AsterismResult]] =
      (pid, oid, _) =>
        services.use { s =>
          s.itcService(itcClient)
           .lookup(pid, oid)
           .map {
             case Left(e)  => Result.failure(e.format)
             case Right(s) => s.result.success
           }
        }

    effectHandler("itc", readEnv, calculate)
  }

}

