// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.effect.Resource
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import eu.timepit.refined.cats.*
import grackle.Env
import grackle.Predicate.True
import grackle.Predicate.and
import grackle.Query
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
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
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.VisitIdBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

trait ExecutionMapping[F[_]] extends ObservationEffectHandler[F] with Predicates[F] {

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
    case (ExecutionType, "datasets", List(
      DatasetIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      Elab.transformChild { child =>
        (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, LIMIT) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                OFFSET.map(Predicates.dataset.id.gtEql).getOrElse(True),
                Predicates.dataset.observation.program.isVisibleTo(user),
              ))),
              oss = Some(List(
                OrderSelection[Dataset.Id](DatasetType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
    case (ExecutionType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      Elab.transformChild { child =>
        (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, LIMIT) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                OFFSET.map(Predicates.executionEvent.id.gtEql).getOrElse(True),
                Predicates.executionEvent.observation.program.isVisibleTo(user),
              ))),
              oss = Some(List(
                OrderSelection[ExecutionEvent.Id](ExecutionEventType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
    case (ExecutionType, "visits", List(
      VisitIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      Elab.transformChild { child =>
        (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, LIMIT) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                OFFSET.map(Predicates.visit.id.gtEql).getOrElse(True),
                Predicates.visit.observation.program.isVisibleTo(user),
              ))),
              oss = Some(List(
                OrderSelection[Visit.Id](VisitType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
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
