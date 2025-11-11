// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.telluric.{TelluricClient, TelluricSearchInput}
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.{Coordinates, Epoch}
import lucuma.core.model.{Observation, Target}
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.util.{TimeSpan, Timestamp}
import lucuma.odb.data.{Existence,  TelluricResolution}
import lucuma.odb.graphql.input.{SiderealInput, TargetPropertiesInput}
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.json.tellurictype.transport.given
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.all.{int4, text, timestamp as tsTimestamp, *}
import skunk.implicits.*
import lucuma.odb.util.Codecs.{calculation_state}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*
import lucuma.odb.service.Services.Syntax.*

trait TelluricResolutionService[F[_]]:

  /**
   * Marks all 'calculating' entries as 'pending' on startup.
   * Called by the daemon to clean up state after restart.
   */
  def reset(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * Loads up to `max` pending/retry resolution entries.
   * Changes state to 'calculating' before returning.
   */
  def load(max: Int)(using ServiceAccess, Transaction[F]): F[List[TelluricResolution.Pending]]

  /**
   * Loads a specific pending resolution entry.
   * Changes state to 'calculating' before returning.
   */
  def loadObs(
    oid: Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Option[TelluricResolution.Pending]]

  /**
   * Resolves the telluric target and updates the database.
   * Must be called outside a transaction (uses TelluricClient).
   */
  def resolveAndUpdate(
    pending: TelluricResolution.Pending
  )(using ServiceAccess, NoTransaction[F]): F[Option[TelluricResolution.Meta]]

  /**
   * Invalidates all telluric targets globally, setting them back to 'pending'.
   * Deletes resolved targets so they revert to placeholders.
   */
  def invalidateAll(using ServiceAccess, Transaction[F]): F[Int]

object TelluricResolutionService:

  def instantiate[F[_]: {Temporal, Logger, Services as S}](
    telluricClient: TelluricClient[F]
  ): TelluricResolutionService[F] =
    new TelluricResolutionService[F]:

      // Exponential backoff calculation
      private def calculateRetryAt(failureCount: Int): FiniteDuration =
        val baseDelay = 30.seconds
        val maxDelay = 1.hour
        val delay = baseDelay * math.pow(2, failureCount.toDouble).toLong
        FiniteDuration(math.min(delay.toSeconds, maxDelay.toSeconds), SECONDS)

      override def reset(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.resetCalculating).void

      override def load(max: Int)(using ServiceAccess, Transaction[F]): F[List[TelluricResolution.Pending]] =
        session
          .prepareR(Statements.loadPending(max))
          .use(_.stream(Void, 1024).compile.toList)

      override def loadObs(
        oid: Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Option[TelluricResolution.Pending]] =
        session
          .prepareR(Statements.loadPendingObs)
          .use(_.option(oid))

      override def resolveAndUpdate(
        pending: TelluricResolution.Pending
      )(using ServiceAccess, NoTransaction[F]): F[Option[TelluricResolution.Meta]] =

        def updateToReady(
          targetId: Option[Target.Id],
          errorMsg: Option[String]
        ): F[Option[TelluricResolution.Meta]] =
          S.transactionally:
            session
              .prepareR(Statements.updateToReady)
              .use(_.option((targetId, errorMsg, pending.observationId, pending.lastInvalidation.toLocalDateTime)))

        def updateToRetry(
          failureCount: Int,
          errorMsg: String
        ): F[Option[TelluricResolution.Meta]] =
          val retryDelay = calculateRetryAt(failureCount)
          S.transactionally:
            session
              .prepareR(Statements.updateToRetry)
              .use(_.option((failureCount + 1, s"${retryDelay.toSeconds} seconds", errorMsg, pending.observationId)))

        def searchTelluricTarget: F[Either[String, Target.Id]] =
          S.transactionally:
            for {
              // Get science target coordinates
              coords   <- session.prepareR(Statements.selectTargetCoordinates)
                             .use(_.option(pending.scienceObservationId))
              // Get F2 config (telluric type + duration)
              f2Config <- session.prepareR(Statements.selectF2Config)
                               .use(_.option(pending.scienceObservationId))
              // Perform search
              result   <- (coords, f2Config) match
                            case (Some(c), Some((telluricType, durationMicros))) =>
                              TimeSpan.fromMicroseconds(durationMicros) match
                                case Some(duration) =>
                                  val searchInput = TelluricSearchInput(
                                    coordinates = c,
                                    duration = duration,
                                    brightest = BigDecimal(8.0),
                                    spType = telluricType
                                  )
                                  telluricClient.search(searchInput).flatMap: stars =>
                                    stars.headOption match
                                      case Some(star) =>
                                        info"Found telluric star HIP ${star.hip} for observation ${pending.observationId}" *>
                                        createTelluricTargetFromStar(pending, star).map(Right(_))
                                      case None =>
                                        val msg = s"No telluric stars found for observation ${pending.observationId}"
                                        info"$msg" *>
                                        Left(msg).pure[F]
                                case None =>
                                  val msg = s"Invalid duration for observation ${pending.observationId}"
                                  warn"$msg" *>
                                  Left(msg).pure[F]
                            case _ =>
                              val msg = s"Missing coordinates or F2 config for observation ${pending.observationId}"
                              info"$msg" *>
                              Left(msg).pure[F]
            } yield result

        def replaceTarget(targetId: Target.Id): F[Unit] =
          S.transactionally:
            Services.asSuperUser:
              for {
                // Get current placeholder target
                oldTargetId <- session.prepareR(Statements.selectTelluricObservationTarget)
                                 .use(_.unique(pending.observationId))
                // Update observation to point to new target
                _ <- session.execute(Statements.updateTelluricObservationTarget)(
                       (targetId, pending.observationId)
                     )
                // Delete old placeholder target
                _ <- session.execute(sql"DELETE FROM t_target WHERE c_target_id = ${target_id}".command)(oldTargetId)
              } yield ()

        def createTelluricTargetFromStar(
          pending: TelluricResolution.Pending,
          star: lucuma.catalog.telluric.TelluricStar
        ): F[Target.Id] =
          S.transactionally:
            Services.asSuperUser:
              val targetInput = TargetPropertiesInput.Create(
                name = NonEmptyString.unsafeFrom(s"HIP ${star.hip}"),
                subtypeInfo = SiderealInput.Create(
                  ra = star.coordinates.ra,
                  dec = star.coordinates.dec,
                  epoch = Epoch.J2000,
                  properMotion = None,
                  radialVelocity = None,
                  parallax = None,
                  catalogInfo = None
                ),
                sourceProfile = SourceProfile.Point(
                  SpectralDefinition.BandNormalized(None, SortedMap.empty)
                ),
                existence = Existence.Present
              )

              targetService.createTarget(
                AccessControl.unchecked(targetInput, pending.programId, program_id),
                disposition = TargetDisposition.Calibration,
                role = CalibrationRole.Telluric.some
              ).orError

        // Main resolution logic
        for
          _      <- debug"Resolving telluric target for observation ${pending.observationId}"
          result <- searchTelluricTarget
          meta   <- result match
            case Right(targetId) =>
              // Success: replace placeholder with real target
              replaceTarget(targetId) *>
              updateToReady(targetId.some, none)
            case Left(errorMsg) if pending.failureCount < 5 =>
              // Transient failure: retry
              warn"Telluric resolution failed (attempt ${pending.failureCount + 1}), will retry: $errorMsg" *>
              updateToRetry(pending.failureCount, errorMsg)
            case Left(errorMsg) =>
              // Permanent failure: mark as ready with error
              error"Telluric resolution permanently failed after ${pending.failureCount} attempts: $errorMsg" *>
              updateToReady(none, errorMsg.some)
        yield meta

      override def invalidateAll(using ServiceAccess, Transaction[F]): F[Int] =
        Services.asSuperUser:
            for
              // Get all telluric observations with resolved targets
              resolved <- session
                            .prepareR(Statements.selectAllResolvedTellurics)
                            .use(_.stream(Void, 1024).compile.toList)
              _        <- info"Invalidating ${resolved.size} resolved telluric targets"
              // Delete resolved targets (will revert to placeholder on next resolution)
              _        <- resolved.traverse: (_, targetId) =>
                             session.execute(sql"DELETE FROM t_target WHERE c_target_id = ${target_id}".command)(targetId)
              // Update all entries to pending state
              count    <- session.execute(Statements.invalidateAllTellurics)
              _        <- info"Invalidated $count telluric resolution entries"
            yield count match {
              case skunk.data.Completion.Update(count) => count.toInt
              case _ => 0
            }

      object Statements:

        val resetCalculating: Command[Void] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'pending',
                   c_retry_at = NULL,
                   c_failure_count = 0
            WHERE  c_state = 'calculating'
          """.command

        def loadPending(max: Int): Query[Void, TelluricResolution.Pending] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'calculating'
            WHERE  c_observation_id IN (
              SELECT c_observation_id
              FROM   t_telluric_resolution
              WHERE  c_state IN ('pending', 'retry')
                AND  (c_retry_at IS NULL OR c_retry_at <= now())
              ORDER BY c_last_invalidation
              LIMIT  $int4
              FOR UPDATE SKIP LOCKED
            )
            RETURNING c_observation_id,
                      c_program_id,
                      c_science_observation_id,
                      c_last_invalidation,
                       c_failure_count
           """.query(observation_id *: program_id *: observation_id *: tsTimestamp *: int4)
               .map { case (oid, pid, scienceOid, lastInv, failCount) =>
                 TelluricResolution.Pending(oid, pid, scienceOid,
                   Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv), failCount)
              }
              .contramap[Void](_ => max)

        val loadPendingObs: Query[Observation.Id, TelluricResolution.Pending] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'calculating'
            WHERE  c_observation_id = $observation_id
              AND  c_state IN ('pending', 'retry')
              AND  (c_retry_at IS NULL OR c_retry_at <= now())
            RETURNING c_observation_id,
                      c_program_id,
                      c_science_observation_id,
                      c_last_invalidation,
                       c_failure_count
""".query(observation_id *: program_id *: observation_id *: tsTimestamp *: int4)
              .map { case (oid, pid, scienceOid, lastInv, failCount) =>
                TelluricResolution.Pending(oid, pid, scienceOid,
                  Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv), failCount)
              }

        val updateToReady: Query[(Option[Target.Id], Option[String], Observation.Id, java.time.LocalDateTime), TelluricResolution.Meta] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'ready',
                   c_last_update = now(),
                   c_resolved_target_id = ${target_id.opt},
                   c_error_message = ${text.opt},
                   c_retry_at = NULL,
                   c_failure_count = 0
            WHERE  c_observation_id = $observation_id
              AND  c_last_invalidation = $tsTimestamp
            RETURNING c_observation_id,
                      c_program_id,
                      c_science_observation_id,
                      c_state,
                      c_last_invalidation,
                      c_last_update,
                      c_retry_at,
                      c_failure_count,
                      c_resolved_target_id,
                      c_error_message
          """.query(
            observation_id *: program_id *: observation_id *: calculation_state *:
            tsTimestamp *: tsTimestamp *: tsTimestamp.opt *: int4 *: target_id.opt *: text.opt
           ).map { case (oid, pid, scienceOid, state, lastInv, lastUpd, retryAt, failCount, targetId, errorMsg) =>
            TelluricResolution.Meta(oid, pid, scienceOid, state,
              Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
              Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
              retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
              failCount, targetId, errorMsg)
         }

        val updateToRetry: Query[(Int, String, String, Observation.Id), TelluricResolution.Meta] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'retry',
                   c_last_update = now(),
                   c_failure_count = $int4,
                   c_retry_at = now() + $text::interval,
                   c_error_message = $text
            WHERE  c_observation_id = $observation_id
            RETURNING c_observation_id,
                      c_program_id,
                      c_science_observation_id,
                      c_state,
                      c_last_invalidation,
                      c_last_update,
                      c_retry_at,
                      c_failure_count,
                      c_resolved_target_id,
                      c_error_message
          """.query(
            observation_id *: program_id *: observation_id *: calculation_state *:
            tsTimestamp *: tsTimestamp *: tsTimestamp.opt *: int4 *: target_id.opt *: text.opt
).map { case (oid, pid, scienceOid, state, lastInv, lastUpd, retryAt, failCount, targetId, errorMsg) =>
              TelluricResolution.Meta(oid, pid, scienceOid, state,
                Timestamp.fromLocalDateTimeTruncatedAndBounded(lastInv),
                Timestamp.fromLocalDateTimeTruncatedAndBounded(lastUpd),
                retryAt.map(t => Timestamp.fromLocalDateTimeTruncatedAndBounded(t)),
                failCount, targetId, errorMsg)
           }

        val selectTargetCoordinates: Query[Observation.Id, Coordinates] =
          sql"""
            SELECT t.c_ra, t.c_dec
            FROM   t_asterism a
            JOIN   t_target t ON t.c_target_id = a.c_target_id
            WHERE  a.c_observation_id = $observation_id
              AND  t.c_sid_ra IS NOT NULL
              AND  t.c_sid_dec IS NOT NULL
            LIMIT  1
          """.query(right_ascension *: declination).map(Coordinates.apply)

        val selectF2Config: Query[Observation.Id, (lucuma.core.model.TelluricType, Long)] =
          sql"""
            SELECT c_telluric_type, c_time_estimate
            FROM   t_flamingos2_long_slit
            WHERE  c_observation_id = $observation_id
          """.query(jsonb.emap(_.as[lucuma.core.model.TelluricType].leftMap(_.getMessage)) *: int8)

        val selectTelluricObservationTarget: Query[Observation.Id, Target.Id] =
          sql"""
            SELECT a.c_target_id
            FROM   t_asterism a
            WHERE  a.c_observation_id = $observation_id
            LIMIT  1
          """.query(target_id)

        val updateTelluricObservationTarget: Command[(Target.Id, Observation.Id)] =
          sql"""
            UPDATE t_asterism
            SET    c_target_id = $target_id
            WHERE  c_observation_id = $observation_id
          """.command

        val selectAllResolvedTellurics: Query[Void, (Observation.Id, Target.Id)] =
          sql"""
            SELECT c_observation_id, c_resolved_target_id
            FROM   t_telluric_resolution
            WHERE  c_resolved_target_id IS NOT NULL
          """.query(observation_id *: target_id)

        val invalidateAllTellurics: Command[Void] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'pending',
                   c_last_invalidation = now(),
                   c_retry_at = NULL,
                   c_failure_count = 0,
                   c_resolved_target_id = NULL,
                   c_error_message = NULL
          """.command
