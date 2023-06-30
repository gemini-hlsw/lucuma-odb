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
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
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

  private val UseCacheParam      = "useCache"
  private val UseCacheDefault    = true
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

        case Select("itc", List(
          BooleanBinding.Option("useCache", rUseCache)
        ), child) =>
          rUseCache.map { useCache =>
            Environment(
              Env(UseCacheParam -> useCache.getOrElse(UseCacheDefault)),
              Select("itc", Nil, child)
            )
          }

        case Select("sequence", List(
          BooleanBinding.Option(UseCacheParam, rUseCache),
          Generator.FutureLimit.Binding.Option(FutureLimitParam, rFutureLimit)
        ), child) =>
          (rUseCache, rFutureLimit).parMapN { case (useCache, futureLimit) =>
            Environment(
              Env(
                UseCacheParam    -> useCache.getOrElse(UseCacheDefault),
                FutureLimitParam -> futureLimit.getOrElse(Generator.FutureLimit.Default)
              ),
              Select("sequence", Nil, child)
            )
          }
      }
    )

  def itcQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Boolean] =
      env => env.getR[Boolean](UseCacheParam)

    val calculate: (Program.Id, Observation.Id, Boolean) => F[Result[ItcService.AsterismResult]] =
      (pid, oid, useCache) =>
        services.use { s =>
          s.itcService(itcClient)
           .lookup(pid, oid, useCache)
           .map {
             case Left(e)  => Result.failure(e.format)
             case Right(s) => s.result.success
           }
        }

    effectHandler("itc", readEnv, calculate)
  }

  def sequenceQueryHandler: EffectHandler[F] = {
    case class SequenceEnv(
      useCache: Boolean,
      limit:    Generator.FutureLimit
    )

    given Eq[SequenceEnv] = Eq.by(a => (a.useCache, a.limit.value))

    val readEnv: Env => Result[SequenceEnv] = env =>
      for {
        c <- env.getR[Boolean](UseCacheParam)
        f <- env.getR[Generator.FutureLimit](FutureLimitParam)
      } yield SequenceEnv(c, f)

    val calculate: (Program.Id, Observation.Id, SequenceEnv) => F[Result[Json]] = (pid, oid, env) => {
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
        s.generator(commitHash, itcClient, plannedTimeCalculator)
         .generate(pid, oid, env.useCache, env.limit)
         .map(_.bimap(mapError, mapSuccess).merge)
      }
    }

    effectHandler("sequence", readEnv, calculate)
  }

  private def effectHandler[E, R](
    fieldName: String,
    readEnv:   Env => Result[E],
    calculate: (Program.Id, Observation.Id, E) => F[Result[R]]
  )(using Eq[E], io.circe.Encoder[R]): EffectHandler[F] =

    new EffectHandler[F] {

      private def queryContext(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id, E)]] =
        queries.traverse { case (_, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
            e <- readEnv(cursor.fullEnv)
          } yield (p, o, e)
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ctx <- ResultT(queryContext(queries).pure[F])
          res <- ctx.distinct.traverse { case (pid, oid, env) =>
                   ResultT(calculate(pid, oid, env)).map((oid, env, _))
                 }
        } yield
          ctx
            .flatMap { case (_, oid, env) => res.find(r => r._1 === oid && r._2 === env).map(_._3).toList }
            .zip(queries)
            .map { case (result, (child, parentCursor)) =>
              val json: Json     = Json.fromFields(List(fieldName -> result.asJson))
              val cursor: Cursor = CirceCursor(parentCursor.context, json, Some(parentCursor), parentCursor.fullEnv)
              (child, cursor)
            }
        ).value

    }

}

