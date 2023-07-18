// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax.*
import eu.timepit.refined.cats.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.syntax.query.*
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services

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

  private val FutureLimitParam   = "futureLimit"

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
        SqlObject("plannedTime"),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id)),
        EffectField("itc", itcQueryHandler, List("id", "programId")),
        EffectField("sequence", sequenceQueryHandler, List("id", "programId"))
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

        case Select("sequence", List(
          BooleanBinding.Option("useCache", rUseCache),
          Generator.FutureLimit.Binding.Option(FutureLimitParam, rFutureLimit)
        ), child) =>
          (rUseCache, rFutureLimit).parMapN { case (_, futureLimit) =>
            Environment(
              Env(FutureLimitParam -> futureLimit.getOrElse(Generator.FutureLimit.Default)),
              Select("sequence", Nil, child)
            )
          }
      }
    )

  def itcQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit, Unit) => F[Result[ItcService.AsterismResult]] =
      (pid, oid, _, _) =>
        services.use { s =>
          s.itcService(itcClient)
           .lookup(pid, oid)
           .map {
             case Left(e)  => Result.failure(e.format)
             case Right(s) => s.result.success
           }
        }

    effectHandler("itc", readEnv, _ => (), calculate)
  }

  def sequenceQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Generator.FutureLimit] =
      _.getR[Generator.FutureLimit](FutureLimitParam)

    val requestsSequence: Query => Boolean =
      _.exists {
        case Select("executionConfig", _, _) => true
        case _                               => false
      }

    val calculate: (Program.Id, Observation.Id, Generator.FutureLimit, Boolean) => F[Result[Json]] =
      (pid, oid, limit, force) => {
        val mapError: Generator.Error => Result[Json] = {
          case Generator.Error.ItcError(ItcService.Error.ObservationNotFound(_, _)) =>
            Result(Json.Null)
          case e: Generator.Error                                                   =>
            Result.failure(e.format)
        }

        def mapSuccess(s: Generator.Success): Result[Json] =
          Json.obj(
            "programId"       -> pid.asJson,
            "observationId"   -> oid.asJson,
            "itcResult"       -> s.itc.asJson,
            "executionConfig" -> s.value.asJson,
            "scienceDigest"   -> s.scienceDigest.asJson
          ).success

        services.use { s =>
          println(force)
          s.generator(commitHash, itcClient, plannedTimeCalculator)
           .generate(pid, oid, limit)
           .map(_.bimap(mapError, mapSuccess).merge)
        }
      }

    effectHandler("sequence", readEnv, requestsSequence, calculate)
  }

  private def effectHandler[E, Q, R](
    fieldName: String,
    readEnv:   Env => Result[E],
    readQuery: Query => Q,
    calculate: (Program.Id, Observation.Id, E, Q) => F[Result[R]]
  )(using Eq[E], Eq[Q], io.circe.Encoder[R]): EffectHandler[F] =

    new EffectHandler[F] {

      private def queryContext(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id, E, Q)]] =
        queries.traverse { case (query, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
            e <- readEnv(cursor.fullEnv)
          } yield (p, o, e, readQuery(query))
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ctx <- ResultT(queryContext(queries).pure[F])
          res <- ctx.distinct.traverse { case (pid, oid, env, q) =>
                   ResultT(calculate(pid, oid, env, q)).map((oid, env, q, _))
                 }
        } yield
          ctx
            .flatMap { case (_, oid, env, q) => res.find(r => r._1 === oid && r._2 === env && r._3 === q).map(_._4).toList }
            .zip(queries)
            .map { case (result, (child, parentCursor)) =>
              val json: Json     = Json.fromFields(List(fieldName -> result.asJson))
              val cursor: Cursor = CirceCursor(parentCursor.context, json, Some(parentCursor), parentCursor.fullEnv)
              (child, cursor)
            }
        ).value

    }

}

