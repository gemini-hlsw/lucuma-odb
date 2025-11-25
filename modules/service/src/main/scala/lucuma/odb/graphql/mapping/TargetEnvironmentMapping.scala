// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.refined.given
import lucuma.catalog.clients.GaiaClient
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.service.GuideService
import lucuma.odb.service.Services
import org.http4s.client.Client

import binding.*
import table.*

trait TargetEnvironmentMapping[F[_]: Temporal]
  extends ObservationEffectHandler[F]
     with AsterismTargetTable[F]
     with ObservationView[F]
     with Predicates[F]
     with TargetView[F] { this: SkunkMapping[F] & TargetMapping[F] =>

  def itcClient: ItcClient[F]
  def httpClient: Client[F]
  def gaiaClient: GaiaClient[F]
  def services: Resource[F, Services[F]]

  private val ObsTimeParam           = "observationTime"
  private val AvailabilityStartParam = "start"
  private val AvailabilityEndParam   = "end"

  private def asterismObject(name: String): SqlObject =
    SqlObject(
      name,
      Join(ObservationView.Id, AsterismTargetTable.ObservationId),
      Join(AsterismTargetTable.TargetId, TargetView.TargetId)
    )

  private def blindOffsetTargetObject(name: String): SqlObject =
    SqlObject(
      name,
      Join(ObservationView.BlindOffsetTargetId, TargetView.TargetId)
    )

  lazy val TargetEnvironmentMapping: ObjectMapping =
    ObjectMapping(TargetEnvironmentType)(
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      asterismObject("asterism"),
      asterismObject("firstScienceTarget"),
      SqlObject("explicitBase"),
      SqlField("useBlindOffset", ObservationView.UseBlindOffset),
      blindOffsetTargetObject("blindOffsetTarget"),
      SqlField("blindOffsetType", ObservationView.BlindOffsetType),
      EffectField("basePosition", basePositionQueryHandler, List("id", "programId")),
      EffectField("guideEnvironments", guideEnvironmentsQueryHandler, List("id", "programId")),
      EffectField("guideEnvironment", guideEnvironmentQueryHandler, List("id", "programId")),
      EffectField("guideAvailability", guideAvailabilityQueryHandler, List("id", "programId")),
      EffectField("guideTargetName", guideTargetNameQueryHandler, List("id", "programId"))
    )

  private def asterismQuery(includeDeleted: Boolean, firstOnly: Boolean, child: Query): Query =
    FilterOrderByOffsetLimit(
      pred   = Some(Predicates.target.existence.includeDeleted(includeDeleted)),
      oss    = List(OrderSelection[Target.Id](TargetType / "id")).some,
      offset = none,
      limit  = Option.when(firstOnly)(1),
      child  = child
    )

  private def blindOffsetTargetQuery(child: Query): Query =
    FilterOrderByOffsetLimit(
      pred   = none, // Blind offset targets are handled via database joins, not GraphQL predicates
      oss    = none,
      offset = none,
      limit  = 1.some,
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

    case (TargetEnvironmentType, "guideAvailability", List(
      TimestampBinding(AvailabilityStartParam, rStart),
      TimestampBinding(AvailabilityEndParam, rEnd)
    )) => for {
      start <- Elab.liftR(rStart)
      end   <- Elab.liftR(rEnd)
      env   <- Elab.env(AvailabilityStartParam -> start, AvailabilityEndParam -> end)
    } yield env

    case (TargetEnvironmentType, "blindOffsetTarget", Nil) =>
      Elab.transformChild { child =>
        Unique(blindOffsetTargetQuery(child))
      }
  }

  def basePositionQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Timestamp] = _.getR[Timestamp](ObsTimeParam)

    val calculate: (Program.Id, Observation.Id, Timestamp) => F[Result[Option[Coordinates]]] =
      (_, oid, obsTime) =>
        services.use { implicit services =>
          Services.asSuperUser:
            services
              .trackingService
              .getCoordinatesSnapshotOrRegion(oid, obsTime, false)
              .map: res =>
                res.map(_.left.toOption.map(_.base)) // treat region as None
        }

    effectHandler(readEnv, calculate)
  }

  def guideEnvironmentsQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Timestamp] = _.getR[Timestamp](ObsTimeParam)

    val calculate: (Program.Id, Observation.Id, Timestamp) => F[Result[List[GuideService.GuideEnvironment]]] =
      (pid, oid, obsTime) =>
        services.use { implicit s =>
          Services.asSuperUser:
            s.guideService
              .getGuideEnvironments(pid, oid, obsTime)
        }

    effectHandler(readEnv, calculate)
  }

  def guideEnvironmentQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[GuideService.GuideEnvironment]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          Services.asSuperUser:
            s.guideService
              .getGuideEnvironment(pid, oid)
        }

    effectHandler(readEnv, calculate)
  }

  def guideAvailabilityQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[TimestampInterval] = env =>
      for {
        start <- env.getR[Timestamp](AvailabilityStartParam)
        end   <- env.getR[Timestamp](AvailabilityEndParam)
        period <- if (start < end) Result.success(TimestampInterval.between(start, end))
                  else Matcher.validationFailure("Start time must be prior to end time for guide star availability")
      } yield period

    val calculate: (Program.Id, Observation.Id, TimestampInterval) => F[Result[List[GuideService.AvailabilityPeriod]]] =
      (pid, oid, period) =>
        services.use { implicit s =>
          Services.asSuperUser:
            s.guideService
              .getGuideAvailability(pid, oid, period)
        }

    effectHandler(readEnv, calculate)
  }

  def guideTargetNameQueryHandler: EffectHandler[F] = {
    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: (Program.Id, Observation.Id, Unit) => F[Result[Option[NonEmptyString]]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          Services.asSuperUser:
            s.guideService
              .getGuideTargetName(pid, oid)
        }

    effectHandler(readEnv, calculate)
  }
}
