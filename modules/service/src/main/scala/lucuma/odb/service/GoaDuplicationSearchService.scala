// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Parallel
import cats.data.NonEmptyChain
import cats.effect.Clock
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.ResultT
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.GoaQueryError
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.GoaDuplication
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.logic.GoaQueryPolicy
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.util.Codecs.*
import skunk.Query
import skunk.syntax.all.*

/**
 * Runs an Archive Duplication Search: turns an observation into GOA queries,
 * executes them and replaces the observation's stored snapshot with what came
 * back.
 *
 * The search is advisory, so a GOA failure is not a failed call.  It is
 * reported as a snapshot whose state is `Error`, carrying the previously stored
 * matches untouched; only a missing observation or a database problem yields a
 * failed `Result`.
 */
trait GoaDuplicationSearchService[F[_]]:

  def refresh(observationId: Observation.Id)(using NoTransaction[F]): F[Result[GoaDuplication.Snapshot]]

object GoaDuplicationSearchService:

  def instantiate[F[_]: Concurrent: Parallel: Clock](
    goaClient: GoaClient[F]
  )(using Services[F]): GoaDuplicationSearchService[F] =
    fromRunner(GoaQueryRunner.fromClient(goaClient))

  def fromRunner[F[_]: Concurrent: Clock](
    runner: GoaQueryRunner[F]
  )(using Services[F]): GoaDuplicationSearchService[F] =
    new GoaDuplicationSearchService[F]:

      import Services.Syntax.*

      /** Everything the query policy needs, as loaded from the database. */
      private case class Context(
        mode:          Option[ObservingMode],
        explicitBase:  Option[Coordinates],
        referenceTime: Option[Timestamp],
        asterism:      List[Target]
      ):
        lazy val pointings: List[GoaQueryPolicy.TargetPointing] =
          asterism.map(GoaQueryPolicy.TargetPointing.fromTarget)

      override def refresh(observationId: Observation.Id)(using NoTransaction[F]): F[Result[GoaDuplication.Snapshot]] =
        (for
          ctx    <- ResultT(loadContext(observationId))
          center <- ResultT.liftF(resolveCenter(observationId, ctx))
          snap   <- ResultT.liftF(search(observationId, ctx, center))
        yield snap).value

      private def loadContext(observationId: Observation.Id)(using NoTransaction[F]): F[Result[Context]] =
        services.transactionally:
          session.option(Statements.SelectObservation)(observationId).flatMap:
            case None                            =>
              OdbError.InvalidObservation(observationId).asFailureF[F, Context]
            case Some((omt, ra, dec, refTime)) =>
              val explicitBase = (ra, dec).mapN(Coordinates.apply)
              for
                mode     <- Services.asSuperUser(omt.traverse: t =>
                              observingModeServices.selectObservingMode(List((observationId, t)))
                            ).map(_.flatMap(_.get(observationId)))
                asterism <- Services.asSuperUser(asterismService.getAsterism(observationId))
              yield Result(Context(mode, explicitBase, refTime, asterism.map(_._2)))

      /**
       * The asterism center, resolved only when the search actually depends on
       * it.  An explicit base or a wholly non-sidereal asterism answers the
       * question on its own, and resolving anyway would mean an ephemeris
       * lookup we do not need.
       *
       * An observation with no scheduled time and no call for proposals has no
       * reference time; it is searched as of now rather than not at all, which
       * for a search this wide only matters to a fast-moving target.  A failed
       * resolution is not an error either — the observation simply reports as
       * not checked.
       */
      private def resolveCenter(observationId: Observation.Id, ctx: Context): F[Option[Coordinates]] =
        if GoaQueryPolicy.searchCenter(ctx.explicitBase, none, ctx.pointings).isDefined then none.pure[F]
        else
          ctx.referenceTime.fold(now)(_.pure[F]).flatMap: t =>
            trackingService
              .getCoordinatesSnapshot(observationId, t, false)
              .map(_.toOption.map(_.base))

      private def search(
        observationId: Observation.Id,
        ctx:           Context,
        center:        Option[Coordinates]
      )(using NoTransaction[F]): F[GoaDuplication.Snapshot] =
        val provenance =
          GoaDuplication.Provenance(
            GoaQueryPolicy.searchCenter(ctx.explicitBase, center, ctx.pointings),
            ctx.mode.flatMap(GoaQueryPolicy.searchRadius)
          )

        val params =
          ctx.mode.toList.flatMap(GoaQueryPolicy.queries(_, ctx.explicitBase, center, ctx.pointings))

        params match
          case Nil => storeNotChecked(observationId, provenance)
          case ps  => runQueries(observationId, provenance, ps)

      /**
       * The observation is one GOA cannot be asked about: an instrument it does
       * not know, or no resolvable pointing.  That is advisory, not a failure.
       */
      private def storeNotChecked(
        observationId: Observation.Id,
        provenance:    GoaDuplication.Provenance
      )(using NoTransaction[F]): F[GoaDuplication.Snapshot] =
        now.flatMap: t =>
          val header = GoaDuplication.Header.notChecked(t, provenance)
          store(observationId, header, Nil).as(GoaDuplication.Snapshot(header, Nil))

      private def runQueries(
        observationId: Observation.Id,
        provenance:    GoaDuplication.Provenance,
        params:        List[GoaParams]
      )(using NoTransaction[F]): F[GoaDuplication.Snapshot] =
        runner.run(params).flatMap:
          case Left(errors)  => storeError(observationId, errors)
          case Right(byQuery) => storeMatches(observationId, provenance, byQuery)

      /**
       * Records the failure without disturbing the stored matches, so a GOA
       * outage leaves the last good snapshot readable alongside the error.
       */
      private def storeError(
        observationId: Observation.Id,
        errors:        NonEmptyChain[GoaQueryError]
      )(using NoTransaction[F]): F[GoaDuplication.Snapshot] =
        val message = errors.toList.map(_.message).mkString("; ")
        services.transactionally:
          goaDuplicationService.storeError(observationId, message) >>
          goaDuplicationService.select(observationId)

      private def storeMatches(
        observationId: Observation.Id,
        provenance:    GoaDuplication.Provenance,
        byQuery:       List[List[GoaSummaryRecord]]
      )(using NoTransaction[F]): F[GoaDuplication.Snapshot] =
        // A file returned by more than one query in the group is one duplicate,
        // not several, so the count is of distinct files.
        val matches = byQuery.flatten.distinctBy(_.name)
        now.flatMap: t =>
          val header =
            GoaDuplication.Header(
              GoaDuplication.State.Checked,
              NonNegInt.unsafeFrom(matches.size),
              // Any query that came back full was truncated, so the count is a floor.
              saturated     = byQuery.exists(_.sizeIs == GoaDuplication.QueryLimit),
              lastCheckedAt = t.some,
              error         = none,
              provenance    = provenance
            )
          store(observationId, header, matches).as(GoaDuplication.Snapshot(header, matches))

      private def store(
        observationId: Observation.Id,
        header:        GoaDuplication.Header,
        matches:       List[GoaSummaryRecord]
      )(using NoTransaction[F]): F[Unit] =
        services.transactionally:
          goaDuplicationService.store(observationId, header, matches)

      private val now: F[Timestamp] =
        Clock[F].realTimeInstant.map(Timestamp.fromInstantTruncatedAndBounded)

  object Statements:

    /** Observing mode, explicit base and reference time, as the policy needs them. */
    type ObservationRow =
      (Option[ObservingModeType], Option[RightAscension], Option[Declination], Option[Timestamp])

    val SelectObservation: Query[Observation.Id, ObservationRow] =
      sql"""
        SELECT
          c_observing_mode_type,
          c_explicit_ra,
          c_explicit_dec,
          COALESCE(c_observation_time, c_reference_time)
        FROM v_observation
        WHERE c_observation_id = $observation_id
      """.query(observing_mode_type.opt *: right_ascension.opt *: declination.opt *: core_timestamp.opt)
