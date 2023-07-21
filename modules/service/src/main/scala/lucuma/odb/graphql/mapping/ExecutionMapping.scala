// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.data.EitherT
import cats.effect.Resource
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.EffectHandler
import edu.gemini.grackle.Query.Environment
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result
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

trait ExecutionMapping[F[_]] extends ObservationEffectHandler[F] {

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
        EffectField("digest", digestHandler, List("id", "programId")),
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

  extension (e: Generator.Error) {
    def toResult: Result[Json] =
      e match {
        case Generator.Error.ItcError(ItcService.Error.ObservationNotFound(_, _)) =>
          Result(Json.Null)
        case e: Generator.Error                                                   =>
          Result.failure(e.format)

      }
  }

  private lazy val configHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Generator.FutureLimit] =
      _.getR[Generator.FutureLimit](FutureLimitParam)

    val calculate: (Program.Id, Observation.Id, Generator.FutureLimit) => F[Result[Json]] =
      (pid, oid, limit) => {
        services.use { s =>
          s.generator(commitHash, itcClient, plannedTimeCalculator)
           .lookupAndGenerate(pid, oid, limit)
           .map(_.bimap(_.toResult, _.config.asJson.success).merge)
        }
      }

    effectHandler("config", readEnv, calculate)
  }

  private lazy val digestHandler: EffectHandler[F] = {
    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Json]] =
      (pid, oid, _) => {
        services.use { s =>
          (for {
            itc <- EitherT(
                     s.itcService(itcClient)
                      .lookup(pid, oid)
                      .map(_.leftMap {
                        case ItcService.Error.ObservationNotFound(_, _) => Result(Json.Null)
                        case e                                          => Result.failure(e.format)
                      })
                   )
            dig <- EitherT(
                     s.generator(commitHash, itcClient, plannedTimeCalculator)
                      .generate(oid, itc.params, itc.result, Generator.FutureLimit.Min)
                      .map(_.bimap(_.toResult, _.config.executionDigest.asJson.success))
                   )
          } yield dig).merge
        }
      }

    effectHandler("digest", _ => ().success, calculate)
  }

}
