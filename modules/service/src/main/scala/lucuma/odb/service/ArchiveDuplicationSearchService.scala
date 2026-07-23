// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Parallel
import cats.data.NonEmptyChain
import cats.effect.Clock
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.GoaQueryError
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ProposalStatus
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.ArchiveDuplication
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.logic.GoaQueryPolicy
import lucuma.odb.otel.ObservationIdKey
import lucuma.odb.otel.given
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Span
import org.typelevel.otel4s.trace.Tracer
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

/**
 * Runs an Archive Duplication Search: turns an observation into GOA queries,
 * executes them and replaces the observation's stored snapshot with what came
 * back.
 */
trait ArchiveDuplicationSearchService[F[_]]:

  /**
   * Re-run the Archive Duplication Search, replacing the observation's stored
   * snapshot with what GOA holds now.
   *
   * `NoTransaction` rather than `Transaction`: the search makes a multi-second
   * outbound call to GOA (and possibly an ephemeris resolution), and running
   * that inside a caller-supplied transaction would pin a connection.
   */
  def refresh(observationId: Observation.Id)(using NoTransaction[F]): F[Result[ArchiveDuplication.Snapshot]]

object ArchiveDuplicationSearchService:

  /**
   * Submission freezes the snapshot, so that the count the TAC and the proposal
   * PDF see is the one the PI last saw.
   */
  extension (ps: ProposalStatus)
    private def isFrozen: Boolean =
      ps >= ProposalStatus.Submitted

  def instantiate[F[_]: {Concurrent, Parallel, Clock, Tracer as T, LoggerFactory as LF}](
    goaClient: GoaClient[F]
  )(using Services[F]): ArchiveDuplicationSearchService[F] =
    new ArchiveDuplicationSearchService[F]:
      val runner = GoaQueryRunner.fromClient(goaClient)

      given Logger[F] = LF.getLoggerFromName("archive-duplication-search")

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

      override def refresh(observationId: Observation.Id)(using NoTransaction[F]): F[Result[ArchiveDuplication.Snapshot]] =
        T.span("archiveDuplicationSearch.refresh", Attribute.from(ObservationIdKey, observationId)).use: span =>
          (for
            ctx    <- ResultT(loadContext(observationId))
            center <- ResultT.liftF(resolveCenter(observationId, ctx))
            snap   <- ResultT.liftF(search(observationId, ctx, center))
          yield snap).value
            .flatTap(recordOutcome(span))
            .onError { case e => span.recordException(e) }

      /** Annotate the span with what the refresh produced, or that it was rejected. */
      private def recordOutcome(span: Span[F])(result: Result[ArchiveDuplication.Snapshot]): F[Unit] =
        result.toOption.fold(
          span.addAttribute(Attribute("archiveDuplication.outcome", "rejected"))
        ): snap =>
          span.addAttributes(
            Attribute("archiveDuplication.outcome", "stored"),
            Attribute("archiveDuplication.state", snap.summary.state.tag),
            Attribute("archiveDuplication.matchCount", snap.summary.matchCount.value.toLong),
            Attribute("archiveDuplication.saturated", snap.summary.saturated)
          )

      private def loadContext(observationId: Observation.Id)(using NoTransaction[F]): F[Result[Context]] =
        services.transactionally:
          session.option(Statements.SelectObservation)(observationId).flatMap:
            case None                                   =>
              OdbError.InvalidObservation(observationId).asFailureF[F, Context]
            case Some((_, _, _, _, ps)) if ps.isFrozen  =>
              OdbError.InvalidObservation(observationId, frozen).asFailureF[F, Context]
            case Some((omt, ra, dec, refTime, _))       =>
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
       */
      private def resolveCenter(observationId: Observation.Id, ctx: Context): F[Option[Coordinates]] =
        if GoaQueryPolicy.searchPointing(ctx.explicitBase, none, ctx.pointings).isDefined then none.pure
        else
          ctx.referenceTime.fold(now)(_.pure[F]).flatMap: t =>
            trackingService
              .getCoordinatesSnapshot(observationId, t, false)
              .map(_.toOption.map(_.base))

      private def search(
        observationId: Observation.Id,
        ctx:           Context,
        center:        Option[Coordinates]
      )(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        val searchArea =
          ArchiveDuplication.SearchArea(
            GoaQueryPolicy.searchPointing(ctx.explicitBase, center, ctx.pointings),
            ctx.mode.flatMap(GoaQueryPolicy.searchRadius)
          )

        val params =
          ctx.mode.toList.flatMap(GoaQueryPolicy.queries(_, ctx.explicitBase, center, ctx.pointings))

        params match
          case Nil => storeNotChecked(observationId, searchArea)
          case ps  => runQueries(observationId, searchArea, ps)

      private def storeNotChecked(
        observationId: Observation.Id,
        searchArea:    ArchiveDuplication.SearchArea
      )(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        now.flatMap: t =>
          val summary = ArchiveDuplication.Summary.notChecked(t, searchArea)
          storeUnlessFrozen(observationId):
            archiveDuplicationService.store(observationId, summary, Nil)
              .as(ArchiveDuplication.Snapshot(summary, Nil))

      private def runQueries(
        observationId: Observation.Id,
        searchArea:    ArchiveDuplication.SearchArea,
        params:        List[GoaParams]
      )(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        info"$observationId: Archive Duplication Search querying GOA: ${params.mkString(" | ")}" *>
          runner.run(params).flatMap:
            case Left(errors)   =>
              info"$observationId: Archive Duplication Search failed: ${errors.toList.map(_.message).mkString("; ")}" *>
                storeError(observationId, errors)
            case Right(byQuery) =>
              info"$observationId: Archive Duplication Search returned ${byQuery.map(_.size).mkString(" + ")} record(s)" *>
                storeMatches(observationId, searchArea, byQuery)

      /**
       * Records the failure without disturbing the stored matches, so a GOA
       * outage leaves the last good snapshot readable alongside the error.
       */
      private def storeError(
        observationId: Observation.Id,
        errors:        NonEmptyChain[GoaQueryError]
      )(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        val message: NonEmptyString =
          NonEmptyString
            .from(errors.toList.map(_.message).mkString("; "))
            .getOrElse("The Archive Duplication Search failed for an unreported reason.".refined)
        storeUnlessFrozen(observationId):
          archiveDuplicationService.storeError(observationId, message) >>
          archiveDuplicationService.select(observationId)

      private def storeMatches(
        observationId: Observation.Id,
        searchArea:    ArchiveDuplication.SearchArea,
        byQuery:       List[List[GoaSummaryRecord]]
      )(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        // A file returned by more than one query in the group is one duplicate,
        // not several, so the count is of distinct files.
        val matches = byQuery.flatten.distinctBy(_.name)
        now.flatMap: t =>
          val summary =
            ArchiveDuplication.Summary(
              ArchiveDuplication.State.Checked,
              NonNegInt.unsafeFrom(matches.size),
              // Any query that came back full was truncated, so the count is a floor.
              saturated     = byQuery.exists(_.sizeIs == ArchiveDuplication.QueryLimit),
              lastCheckedAt = t.some,
              error         = none,
              searchArea    = searchArea
            )
          storeUnlessFrozen(observationId):
            archiveDuplicationService.store(observationId, summary, matches)
              .as(ArchiveDuplication.Snapshot(summary, matches))

      /**
       * Applies a snapshot write, but only while the proposal is still ours to
       * replace.  `loadContext` already rejects a frozen proposal, but the
       * multi-second GOA call runs between that check and this write, so a
       * proposal submitted in the meantime would otherwise overwrite the frozen
       * snapshot the TAC is meant to see.  Re-reading the status here — inside
       * the writing transaction and behind a `FOR UPDATE` lock on the program
       * row, which serialises against the submission `UPDATE` — makes the freeze
       * hold at the one place that writes the snapshot, as the ADR requires.  A
       * refused write returns whatever is currently stored.
       */
      private def storeUnlessFrozen(
        observationId: Observation.Id
      )(write: Transaction[F] ?=> F[ArchiveDuplication.Snapshot])(using NoTransaction[F]): F[ArchiveDuplication.Snapshot] =
        services.transactionally:
          session.option(Statements.LockProposalStatus)(observationId).flatMap:
            case Some(ps) if !ps.isFrozen => write
            case _                        => archiveDuplicationService.select(observationId)

      private val now: F[Timestamp] =
        Clock[F].realTimeInstant.map(Timestamp.fromInstantTruncatedAndBounded)

      private val frozen: Option[String] =
        "The Archive Duplication Search cannot be re-run because the proposal has been submitted.".some

  object Statements:

    /**
     * Observing mode, explicit base and reference time, as the policy needs
     * them, plus the proposal status, which decides whether the snapshot is
     * still ours to replace.
     */
    type ObservationRow =
      (Option[ObservingModeType], Option[RightAscension], Option[Declination], Option[Timestamp], ProposalStatus)

    val SelectObservation: Query[Observation.Id, ObservationRow] =
      sql"""
        SELECT
          o.c_observing_mode_type,
          o.c_explicit_ra,
          o.c_explicit_dec,
          COALESCE(o.c_observation_time, o.c_reference_time),
          p.c_proposal_status
        FROM v_observation o
        JOIN t_program p ON p.c_program_id = o.c_program_id
        WHERE o.c_observation_id = $observation_id
      """.query(observing_mode_type.opt *: right_ascension.opt *: declination.opt *: core_timestamp.opt *: proposal_status)

    /**
     * The observation's proposal status, taking a row lock on the program so a
     * submission that lands during the GOA call cannot slip in between this read
     * and the snapshot write that follows it in the same transaction.
     */
    val LockProposalStatus: Query[Observation.Id, ProposalStatus] =
      sql"""
        SELECT p.c_proposal_status
        FROM t_observation o
        JOIN t_program p ON p.c_program_id = o.c_program_id
        WHERE o.c_observation_id = $observation_id
        FOR UPDATE OF p
      """.query(proposal_status)
