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
import cats.syntax.traverse.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.EffectHandler
import edu.gemini.grackle.Query.Environment
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.syntax.*
import eu.timepit.refined.cats.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.client.ItcClient
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.Services

import table.ObservationView

trait ExecutionMapping[F[_]] extends ObservationView[F] {

  private val FutureLimitParam   = "futureLimit"

  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode

  lazy val ExecutionMapping: ObjectMapping =
    ObjectMapping(
      tpe = ExecutionType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlField("programId", ObservationView.ProgramId, hidden = true),
        EffectField("config", configHandler, List("id", "programId"))
      )
    )

  lazy val ExecutionElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ExecutionType -> {
        case Select("config", List(
          Generator.FutureLimit.Binding.Option(FutureLimitParam, rFutureLimit)
        ), child) =>
          rFutureLimit.map { futureLimit =>
            Environment(
              Env(FutureLimitParam -> futureLimit.getOrElse(Generator.FutureLimit.Default)),
              Select("config", Nil, child)
            )
          }
      }
    )

  private lazy val configHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Generator.FutureLimit] =
      _.getR[Generator.FutureLimit](FutureLimitParam)

    val calculate: (Program.Id, Observation.Id, Generator.FutureLimit) => F[Result[Json]] =
      (pid, oid, limit) => {
        val mapError: Generator.Error => Result[Json] = {
          case Generator.Error.ItcError(ItcService.Error.ObservationNotFound(_, _)) =>
            Result(Json.Null)
          case e: Generator.Error                                                   =>
            Result.failure(e.format)
        }

        services.use { s =>
          s.generator(commitHash, itcClient, plannedTimeCalculator)
           .generate(pid, oid, limit)
           .map(_.bimap(mapError, _.value.asJson.success).merge)
        }
      }

    effectHandler("config", readEnv, calculate)
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
