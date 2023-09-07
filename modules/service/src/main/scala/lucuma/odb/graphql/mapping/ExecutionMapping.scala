// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.EffectHandler
import edu.gemini.grackle.Query.Environment
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
import lucuma.odb.service.Services
import edu.gemini.grackle.QueryCompiler.Elab
import edu.gemini.grackle.Query.Binding
import edu.gemini.grackle.Env

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

  lazy val ExecutionElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ExecutionType, "config", List(Generator.FutureLimit.Binding.Option(FutureLimitParam, rFutureLimit))) =>
      Elab.transformChild { child =>
        rFutureLimit.map { futureLimit =>
          Environment(
            Env(FutureLimitParam -> futureLimit.getOrElse(Generator.FutureLimit.Default)),
            child
          )
        }
      }

  extension (e: Generator.Error) {
    def toResult: Result[Json] =
      Result.failure(e.format)
  }

  private lazy val configHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Generator.FutureLimit] =
      _.getR[Generator.FutureLimit](FutureLimitParam)

    val calculate: (Program.Id, Observation.Id, Generator.FutureLimit) => F[Result[Json]] =
      (pid, oid, limit) => {
        services.use { s =>
          s.generator(commitHash, itcClient, plannedTimeCalculator)
           .generate(pid, oid, limit)
           .map(_.bimap(_.toResult, _.asJson.success).merge)
        }
      }

    effectHandler("config", readEnv, calculate)
  }

  private lazy val digestHandler: EffectHandler[F] = {
    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Json]] =
      (pid, oid, _) => {
        services.use { s =>
          s.generator(commitHash, itcClient, plannedTimeCalculator)
           .digest(pid, oid)
           .map(_.bimap(_.toResult, _.asJson.success).merge)
        }
      }

    effectHandler("digest", _ => ().success, calculate)
  }

}
