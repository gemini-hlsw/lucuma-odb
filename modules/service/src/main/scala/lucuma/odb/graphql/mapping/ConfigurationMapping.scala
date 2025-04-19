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
import grackle.Result.Failure
import grackle.Result.Warning
import grackle.ResultT
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import org.http4s.client.Client

trait ConfigurationMapping[F[_]]
  extends ObservationView[F] with ConfigurationRequestView[F] {

  def services: Resource[F, Services[F]]
  def itcClient: ItcClient[F]
  def httpClient: Client[F]
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
      SqlObject("conditions"),
      SqlObject("referenceCoordinates"),
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

      SqlObject("conditions"),
      EffectField("referenceCoordinates", referencePositionQueryHandler, List("id", "programId", "referenceTime")),
      SqlObject("observingMode"),
    )

  // Use GuideService.getObjectTrackikng to compute thet location of this observation's asterism at the middle
  // of the CFP's active period (if we can).
  def referencePositionQueryHandler: EffectHandler[F] =

    // N.B. we can't use ObservationEffectHandler here because it doesn't gather all the infomation we need from
    // the cursor, and we don't need the environment at all. Would be nice to abstract something out.
    new EffectHandler[F] {

      def calculate(pid: Program.Id, oid: Observation.Id, oRefTime: Option[Timestamp]): F[Result[Option[Coordinates]]] =
        oRefTime match
          case None     =>
            // If there is no reference time then we can't compute the reference coordinates in general,
            // although we could do it for sidereal asterisms with no proper motion. At this point I don't
            // think it's worthwhile; this computation is only meaningful for fully-defined programs.
            Result(None).pure[F]
          case Some(refTime) =>
            services.use { implicit s =>
              s.guideService(httpClient, itcClient, commitHash, timeEstimateCalculator)
                .getObjectTracking(pid, oid)
                .map:
                  case Failure(problems) => Warning(problems, None) // turn failure into a warning
                  case other => other.map(_.at(refTime.toInstant).map(_.value))
            }

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
                   ResultT(calculate(pid, oid, ldt)).map((oid, _))
                 }
          res <- ResultT(ctx
                   .flatMap { case (pid, oid, ldt) => obs.find(r => r._1 === oid).map(_._2).toList }
                   .zip(queries)
                   .traverse { case (result, (query, parentCursor)) =>
                     Query.childContext(parentCursor.context, query).map { childContext =>
                       CirceCursor(childContext, result.asJson, Some(parentCursor), parentCursor.fullEnv)
                     }
                   }.pure[F]
                 )
          } yield res
        ).value

    }


}
