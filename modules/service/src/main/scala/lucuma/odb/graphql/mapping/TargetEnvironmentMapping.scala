// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import grackle.Env
import grackle.Query
import grackle.Query._
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.AsterismTargetTable
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.GuideEnvironmentService
import lucuma.odb.service.Services
import org.http4s.client.Client

import binding._
import table._

trait TargetEnvironmentMapping[F[_]: Temporal]
  extends ObservationEffectHandler[F] 
     with AsterismTargetTable[F]
     with ObservationView[F]
     with Predicates[F]
     with TargetView[F] { this: SkunkMapping[F] with TargetMapping[F] =>

  def itcClient: ItcClient[F]
  def httpClient: Client[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode

  private val ObsTimeParam = "observationTime"

  private def asterismObject(name: String): SqlObject =
    SqlObject(
      name,
      Join(ObservationView.Id, AsterismTargetTable.ObservationId),
      Join(AsterismTargetTable.TargetId, TargetView.TargetId)
    )

  lazy val TargetEnvironmentMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetEnvironmentType,
      fieldMappings = List(
        SqlField("programId", ObservationView.ProgramId, hidden = true),
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        asterismObject("asterism"),
        asterismObject("firstScienceTarget"),
        SqlObject("explicitBase"),
        EffectField("guideEnvironments", guideEnvironmentQueryHandler, List("id", "programId"))
      )
    )

  private def asterismQuery(includeDeleted: Boolean, firstOnly: Boolean, child: Query): Query =
    FilterOrderByOffsetLimit(
      pred   = Some(Predicates.target.existence.includeDeleted(includeDeleted)),
      oss    = List(OrderSelection[Target.Id](TargetType / "id")).some,
      offset = none,
      limit  = Option.when(firstOnly)(1),
      child  = child
    )

  lazy val TargetEnvironmentElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (TargetEnvironmentType, "asterism", List(BooleanBinding("includeDeleted", rIncludeDeleted))) =>
      Elab.transformChild { child =>
        rIncludeDeleted.map { includeDeleted =>
          asterismQuery(includeDeleted, firstOnly = false, child)
        }
      }

    // TODO: not yet working
    case (TargetEnvironmentType, "firstScienceTarget", List(
      BooleanBinding("includeDeleted", rIncludeDeleted)
    )) =>
      Elab.transformChild { child =>
        rIncludeDeleted.map { includeDeleted =>
          Unique(asterismQuery(includeDeleted, firstOnly = true, child))
        }
      }

    case (TargetEnvironmentType, "guideEnvironments", List(
      TimestampBinding(ObsTimeParam, rObsTime)
    )) => 
      Elab.liftR(rObsTime).flatMap { obsTime =>
        Elab.env(ObsTimeParam -> obsTime)
      }
  }

  def guideEnvironmentQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Timestamp] = _.getR[Timestamp](ObsTimeParam)

    val calculate: (Program.Id, Observation.Id, Timestamp) => F[Result[List[GuideEnvironmentService.GuideEnvironment]]] =
      (pid, oid, obsTime) =>
        services.use { s =>
          s.guideEnvironmentService(httpClient, itcClient, commitHash, plannedTimeCalculator)
            .get(pid, oid, obsTime)
            .map {
              case Left(e)  => Result.failure(e.format)
              case Right(s) => s.success
            }
        }

    effectHandler("guideEnvironments", readEnv, calculate)
  }
}

