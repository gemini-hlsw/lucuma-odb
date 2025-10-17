// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.cats.*
import grackle.Env
import grackle.Query
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.TimestampBinding
import lucuma.odb.graphql.binding.VisitIdBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ObscalcTable
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

trait ExecutionMapping[F[_]: Logger] extends ObservationEffectHandler[F]
                                with ObscalcTable[F]
                                with Predicates[F]
                                with SelectSubquery {

  private val FutureLimitParam = "futureLimit"
  private val ResetAcqParam    = "reset"

  def user: User
  def itcClient: ItcClient[F]
  def httpClient: Client[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode

  lazy val ExecutionMapping: ObjectMapping =
    ObjectMapping(ExecutionType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      EffectField("digest", digestHandler, List("id", "programId")),
      EffectField("config", configHandler, List("id", "programId")),
      EffectField("executionState",  executionStateHandler, List("id", "programId")),
      SqlObject("atomRecords"),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("visits"),
      EffectField("timeCharge", timeChargeHandler, List("id", "programId"))
    )

  lazy val ExecutionElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (GmosNorthExecutionConfigType, "acquisition", List(
      BooleanBinding.Option(ResetAcqParam, rReset)
    )) =>
      Elab.liftR(rReset).flatMap: reset =>
        Elab.env(ResetAcqParam -> reset.getOrElse(false))

    case (GmosSouthExecutionConfigType, "acquisition", List(
      BooleanBinding.Option(ResetAcqParam, rReset)
    )) =>
      Elab.liftR(rReset).flatMap: reset =>
        Elab.env(ResetAcqParam -> reset.getOrElse(false))

    case (ExecutionType, "config", List(
      Generator.FutureLimit.Binding.Option(FutureLimitParam, rFutureLimit)
    )) =>
      Elab.liftR(rFutureLimit).flatMap { futureLimit =>
        Elab.env(FutureLimitParam -> futureLimit.getOrElse(Generator.FutureLimit.Default))
      }

    case (ExecutionType, "atomRecords", List(
      TimestampBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, AtomRecordType, "created", Predicates.atomRecord.created, Predicates.atomRecord.visit.observation.program)

    case (ExecutionType, "datasets", List(
      DatasetIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, DatasetType, "id", Predicates.dataset.id, Predicates.dataset.observation.program)

    case (ExecutionType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, ExecutionEventType, "id", Predicates.executionEvent.id, Predicates.executionEvent.observation.program)

    case (ExecutionType, "visits", List(
      VisitIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, VisitType, "id", Predicates.visit.id, Predicates.visit.observation.program)

  }

  private lazy val configHandler: EffectHandler[F] =

    val readEnv: Env => Result[Generator.FutureLimit] = env =>
      env.getR[Generator.FutureLimit](FutureLimitParam)

    val calculate: (Program.Id, Observation.Id, Generator.FutureLimit) => F[Result[Json]] =
      (pid, oid, futureLimit) =>
        services.use: s =>
          Services.asSuperUser:
            s.generator(commitHash, itcClient, timeEstimateCalculator)
             .generate(pid, oid, futureLimit)
             .map(_.bimap(_.asWarning(Json.Null), _.asJson.success).merge)

    // Scans the top-level query and its descendents for environment entries,
    // merges them with the top-level query environment.  This is done in order
    // to pick up the 'reset' parameter on acquisition sequence generation.
    def buildEnv(env: Env, query: Query): Env =
      Query.children(query).foldLeft(env)((e, child) => buildEnv(e.addFromQuery(child), child))

    readQueryAndCursorEffectHander(
      (q, c) => readEnv(buildEnv(c.fullEnv, q)),
      calculate
    )

  private lazy val executionStateHandler: EffectHandler[F] =
    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Json]] =
      (pid, oid, _) => {
        services.use: s =>
          Services.asSuperUser:
            s.generator(commitHash, itcClient, timeEstimateCalculator)
             .executionState(pid, oid)
             .map(_.asJson.success)
      }

    effectHandler(_ => ().success, calculate)

  private lazy val digestHandler: EffectHandler[F] =
    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Json]] =
      (_, oid, _) =>
        services.useTransactionally:
          obscalcService(commitHash, itcClient, httpClient, timeEstimateCalculator)
           .selectExecutionDigest(oid)
           .map:
             _.fold(OdbError.SequenceUnavailable(oid).asWarning(Json.Null)): cv =>
               cv
                 .map: res =>
                   res.map(_.asJson) match
                     case Result.Failure(ps) => Result.Warning(ps, Json.Null)
                     case r                  => r
                 .sequence.map(_.asJson)

    effectHandler(_ => ().success, calculate)

  private lazy val timeChargeHandler: EffectHandler[F] = {
    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Json]] =
      (_, oid, _) => {
        services.useTransactionally {
          Services.asSuperUser:
            timeAccountingService.selectObservation(oid).map(_.asJson.success)
        }
      }

    effectHandler(_ => ().success, calculate)
  }

}
