// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarName
import lucuma.catalog.clients.GaiaClient
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

/**
 * Resolves the guide star the PI stored for an observation against Gaia, yielding what Altair needs
 * from it: how far off axis it is and how bright.
 *
 * This lives apart from [[GuideService]] so that anything needing only the stored star can use it
 * without pulling in AGS: `GuideService` is built on the generator and its methods demand
 * `NoTransaction`, while nothing here does.
 */
trait GuideStarResolver[F[_]]:

  /**
   * The stored guide star, or None when the observation has none. The stored hash is deliberately
   * ignored: the ITC uses the star that was picked even when the sequence has since changed.
   */
  def resolve(oid: Observation.Id): F[Result[Option[GuideStarResolver.ResolvedGuideStar]]]

  /** [[resolve]] over a batch of observations. */
  def resolveAll(oids: List[Observation.Id]): F[Map[Observation.Id, Result[Option[GuideStarResolver.ResolvedGuideStar]]]]

object GuideStarResolver:

  /** A stored guide star, with what Altair needs from it: how far off axis it is and how bright. */
  case class ResolvedGuideStar(
    name:        GuideStarName,
    separation:  Angle,
    rBrightness: Option[BrightnessValue]
  )

  private def error(msg: String): OdbError =
    OdbError.GuideEnvironmentError(msg.some)

  def instantiate[F[_]: Sync](gaiaClient: GaiaClient[F])(using Services[F]): GuideStarResolver[F] =
    new GuideStarResolver[F]:

      // The stored guide star is resolved on every ITC call, and the star itself never changes,
      // so one Gaia query per source id is enough.
      private val storedGuideStars: Ref[F, Map[Long, GuideStarCandidate]] =
        Ref.unsafe(Map.empty)

      private def selectStoredStar(
        oid: Observation.Id
      ): F[Option[(GuideStarName, Option[Timestamp], Option[Coordinates])]] =
        val af: AppliedFragment = Statements.selectStoredGuideStar(oid)
        session
          .prepareR(af.fragment.query(guide_target_name *: core_timestamp.opt *: right_ascension.opt *: declination.opt))
          .use(_.option(af.argument))
          .map(_.map((name, obsTime, ra, dec) => (name, obsTime, (ra, dec).mapN(Coordinates.apply))))

      private def gaiaCandidate(name: GuideStarName): F[Result[GuideStarCandidate]] =
        (for
          id        <- ResultT.fromResult(name.toGaiaSourceId.toResult(error(s"Invalid guide star name '$name'").asProblem))
          cached    <- ResultT.liftF(storedGuideStars.get.map(_.get(id)))
          candidate <- cached.fold(
                         ResultT(
                           gaiaClient
                             .queryById(id)
                             .map:
                               _.toOption
                                 .map(s => GuideStarCandidate.siderealTarget.get(s.target))
                                 .toResult(error(s"Star with id $id not found on Gaia.").asProblem)
                             .handleError(e => error(s"Error calling Gaia: ${e.getMessage}").asFailure)
                         ).flatMap(star => ResultT.liftF(storedGuideStars.update(_.updated(id, star))).as(star))
                       )(ResultT.pure)
        yield candidate).value

      override def resolve(oid: Observation.Id): F[Result[Option[ResolvedGuideStar]]] =
        (for
          stored   <- ResultT.liftF(selectStoredStar(oid))
          resolved <- stored.flatTraverse(resolveStar(oid, _, _, _))
        yield resolved).value

      // Without an observation time there is no epoch at which to place the star, so it counts as
      // not yet resolved rather than as an error.
      private def resolveStar(
        oid:          Observation.Id,
        name:         GuideStarName,
        optObsTime:   Option[Timestamp],
        explicitBase: Option[Coordinates]
      ): ResultT[F, Option[ResolvedGuideStar]] =
        optObsTime.traverse: obsTime =>
          for
            candidate  <- ResultT(gaiaCandidate(name)).map(_.at(obsTime.toInstant)) // PM corrected
            tracking   <- ResultT(trackingService.getTrackingSnapshot(oid, TimestampInterval.empty(obsTime), false))
            baseCoords <- ResultT.fromResult(
                            explicitBase
                              .orElse(tracking.base.at(obsTime.toInstant))
                              .toResult(error(s"Unable to get coordinates for asterism in observation $oid").asProblem)
                          )
          yield ResolvedGuideStar(
            name,
            baseCoords.angularDistance(candidate.tracking.baseCoordinates),
            candidate.rBrightness
          )

      override def resolveAll(
        oids: List[Observation.Id]
      ): F[Map[Observation.Id, Result[Option[ResolvedGuideStar]]]] =
        oids.distinct.traverse(oid => resolve(oid).tupleLeft(oid)).map(_.toMap)

  object Statements:

    def selectStoredGuideStar(oid: Observation.Id): AppliedFragment =
      sql"""
        SELECT
          c_guide_target_name,
          c_observation_time,
          c_explicit_ra,
          c_explicit_dec
        FROM t_observation
        WHERE c_observation_id       = $observation_id
          AND c_guide_target_name IS NOT NULL
      """.apply(oid)
