// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.all.*
import grackle.Cursor
import grackle.Query
import grackle.Query.EffectHandler
import grackle.Result
import grackle.ResultT
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.region.query.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import org.http4s.client.Client

trait ConfigurationMapping[F[_]]
  extends ObservationView[F] with ConfigurationRequestView[F] {

  def services: Resource[F, Services[F]]
  def itcClient: ItcClient[F]
  def httpClient: Client[F]
  def gaiaClient: GaiaClient[F]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode

  lazy val ConfigurationMappings =
    List(
      ObservationConfigurationMapping,
      ConfigurationRequestConfigurationMapping,
    )

  private lazy val ConfigurationRequestConfigurationMapping: ObjectMapping =
    ObjectMapping(ConfigurationRequestType / "configuration")(
      SqlField("synthetic-id", ConfigurationRequestView.Id, key = true, hidden = true),
      SqlObject("target"),
      SqlObject("conditions"),
      SqlObject("observingMode"),
    )

  private lazy val ObservationConfigurationMapping: ObjectMapping =
    ObjectMapping(ObservationType / "configuration")(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),

      // N.B. the reference time here has no zone information, so it's interpreted as UTC. This will
      // almost certainly never matter, but if it does matter at some point then we can change it
      // to use the timezone of the site associated with the instrument. In any case this value is
      // a computed column in the view and is the midpoint of the active period for the observation's
      // associated CFP (if any). The reference time is used when computing coordintes for approved
      // configurations; it is not related to visualization time.
      SqlField("referenceTime", ObservationView.ReferenceTime, hidden = true),
      EffectField("target", targetQueryHandler, List("id", "programId", "referenceTime")),
      SqlObject("conditions"),
      SqlObject("observingMode"),
    )

  def targetQueryHandler: EffectHandler[F] =

    // N.B. we can't use ObservationEffectHandler here because it doesn't gather all the infomation we need from
    // the cursor, and we don't need the environment at all. Would be nice to abstract something out.
    new EffectHandler[F] {

      def calculate(oid: Observation.Id, oRefTime: Option[Timestamp]): F[Result[Option[Either[Coordinates, Region]]]] =
        oRefTime
          .traverse: at =>
            services.use { implicit s =>
              Services.asSuperUser:
                s.trackingService
                  .getCoordinatesSnapshotOrRegion(oid, at, false)
                  .map: res =>
                    if res.isFailure then Result(None) // important, don't fail here
                    else res
                      .map:
                        case Left(a)             => Left(a.base).some // non-opportunity
                        case Right((_, Some(c))) => Left(c).some      // opportunity with explicit base
                        case Right((r, None))    => Right(r).some     // opportunity without explicit base
              }
          .map(_.sequence.map(_.flatten))

      private def queryContext(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id, Option[Timestamp])]] =
        queries.parTraverse: (_, cursor) =>
          (
            cursor.fieldAs[Program.Id]("programId"),
            cursor.fieldAs[Observation.Id]("id"),
            cursor.fieldAs[Option[Timestamp]]("referenceTime")
          ).parTupled

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          ctx <- ResultT(queryContext(queries).pure[F])
          obs <- ctx.distinct.traverse { case (pid, oid, ldt) =>
                   ResultT(calculate(oid, ldt)).map((oid, _))
                 }
          res <- ResultT(ctx
                   .flatMap { case (pid, oid, ldt) => obs.find(r => r._1 === oid).map(_._2).toList }
                   .zip(queries)
                   .traverse { case (result, (query, parentCursor)) =>
                     Query.childContext(parentCursor.context, query).map { childContext =>
                       CirceCursor(
                        childContext,
                        Json.obj(
                          "coordinates" -> result.flatMap(_.left.toOption).asJson,
                          "region"      -> result.flatMap(_.toOption).asJson,
                        ),
                        Some(parentCursor),
                        parentCursor.fullEnv
                      )
                     }
                   }.pure[F]
                 )
          } yield res
        ).value

    }


}
