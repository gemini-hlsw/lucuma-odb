// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.logic.Itc
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import table.ObsAttachmentAssignmentTable
import table.ObsAttachmentTable
import table.ObservationView
import table.ProgramTable


trait ObservationMapping[F[_]]
  extends ObservationView[F]
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
        SqlObject("plannedTime"),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id)),
        EffectField("itc", itcQueryHandler, List("id", "programId"))
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
      }
    )

  def itcQueryHandler: EffectHandler[F] =
    new EffectHandler[F] {
      private def extractIds(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id)]] =
        queries.traverse { case (_, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
          } yield (p, o)
        }

      private def callItc(pid: Program.Id, oid: Observation.Id): F[Result[(Observation.Id, Itc.ResultSet)]] =
        services.useTransactionally {
          itc(itcClient)
            .lookup(pid, oid, useCache = true)
            .map {
              case Left(errors)     => Result.failure(errors.map(_.format).intercalate(", "))
              case Right(resultSet) => (oid, resultSet).success
            }
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ids <- ResultT(extractIds(queries).pure[F])
          itc <- ids.distinct.traverse { case (pid, oid) => ResultT(callItc(pid, oid)) }
        } yield
          ids
            .flatMap { case (_, oid) => itc.find(_._1 === oid).map(_._2).toList }
            .zip(queries)
            .map { case (itcResultSet, (child, parentCursor)) =>
              val itc: Json      = Json.fromFields(List("itc" -> itcResultSet.asJson))
              val cursor: Cursor = CirceCursor(parentCursor.context, itc, Some(parentCursor), parentCursor.fullEnv)
              (child, cursor)
            }
        ).value
    }

}

