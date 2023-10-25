// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
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
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.TimestampBinding
import lucuma.odb.graphql.binding.VisitIdBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

trait ExecutionMapping[F[_]] extends ObservationEffectHandler[F]
                                with Predicates[F]
                                with SelectSubquery {

  private val FutureLimitParam   = "futureLimit"

  def user: User
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
        EffectField("config", configHandler, List("id", "programId")),
        SqlObject("atomRecords"),
        SqlObject("datasets"),
        SqlObject("events"),
        SqlObject("visits")
      )
    )

  lazy val ExecutionElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

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
