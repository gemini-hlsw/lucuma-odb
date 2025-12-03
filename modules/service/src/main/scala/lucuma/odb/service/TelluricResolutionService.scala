// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.TelluricResolution
import lucuma.odb.graphql.input.CatalogInfoInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Codecs.calculation_state
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import skunk.*
import skunk.codec.all.{timestamp as tsTimestamp, *}
import skunk.implicits.*

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

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
  def load(max: Int): F[List[TelluricResolution.Pending]]

  /**
   * Loads a specific pending resolution entry.
   * Changes state to 'calculating' before returning.
   */
  def loadObs(
    oid: Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Option[TelluricResolution.Pending]]

  /**
   * Records a new telluric resolution request.
   * Called when a telluric observation is created.
   */
  def recordResolutionRequest(
    pid:        Program.Id,
    telluricId: Observation.Id,
    scienceId:  Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * Resolves the telluric target and updates the database.
   * Must be called outside a transaction (uses TelluricTargetsClient).
   */
  def resolveAndUpdate(
    pending: TelluricResolution.Pending
  )(using ServiceAccess, NoTransaction[F]): F[Option[TelluricResolution.Meta]]

  /**
   * Rechecks telluric resolution for observations linked to a science observation.
   * Re-queries for telluric star and only replaces target if different.
   * Called when science observation changes (obscalc ready).
   */
  def recheckForScienceObservation(
    scienceObsId: Observation.Id
  )(using ServiceAccess, NoTransaction[F]): F[Unit]

object TelluricResolutionService:

  /** Convert a TelluricStar to a Target.Sidereal */
  def toSiderealTarget(star: TelluricStar): Target.Sidereal =
    Target.Sidereal(
      name = NonEmptyString.unsafeFrom(s"HIP ${star.hip}"),
      tracking = SiderealTracking(
        baseCoordinates = star.coordinates,
        epoch = Epoch.J2000,
        properMotion = None,
        radialVelocity = None,
        parallax = None
      ),
      sourceProfile = SourceProfile.Point(
        SpectralDefinition.BandNormalized(None, SortedMap.empty)
      ),
      catalogInfo = None
    )

  def instantiate[F[_]: {Temporal, LoggerFactory as LF, Services as S}](
    telluricClient: TelluricTargetsClient[F]
  ): TelluricResolutionService[F] =
    new TelluricResolutionService[F]:
      given Logger[F] = LF.getLoggerFromName("telluric-targets")

      // Exponential backoff calculation
      private def calculateRetryAt(failureCount: Int): FiniteDuration =
        val baseDelay = 30.seconds
        val maxDelay = 1.hour
        val delay = baseDelay * math.pow(2, failureCount.toDouble).toLong
        FiniteDuration(math.min(delay.toSeconds, maxDelay.toSeconds), SECONDS)

      override def reset(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.resetCalculating).void

      override def load(max: Int): F[List[TelluricResolution.Pending]] =
        session
          .prepareR(Statements.loadPending(max))
          .use(_.stream(Void, 1024).compile.toList)

      override def loadObs(
        oid: Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Option[TelluricResolution.Pending]] =
        session
          .prepareR(Statements.loadPendingObs)
          .use(_.option(oid))

      override def recordResolutionRequest(
        pid:        Program.Id,
        telluricId: Observation.Id,
        scienceId:  Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.insertResolutionRequest)(
          telluricId, pid, scienceId
        ).void

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

        // Query data needed for telluric search (in transaction)
        def querySearchData: F[Either[String, (Coordinates, lucuma.core.model.TelluricType, TimeSpan)]] =
          S.transactionally:
            Services.asSuperUser:
              for
                coords   <- session.prepareR(Statements.selectTargetCoordinates)
                               .use(_.option(pending.scienceObservationId))
                f2Config <- flamingos2LongSlitService.select(List(pending.scienceObservationId))
                               .map(_.get(pending.scienceObservationId))
                duration <- session.prepareR(Statements.selectObservationDuration)
                               .use(_.option(pending.scienceObservationId))
                _        <- {
                              println(s"Telluric resolution for ${pending.observationId}: coords=${coords.isDefined}, f2Config=${f2Config.isDefined}, duration=${duration.isDefined}, scienceObs=${pending.scienceObservationId}")
                              info"Telluric resolution for ${pending.observationId}: coords=${coords.isDefined}, f2Config=${f2Config.isDefined}, duration=${duration.isDefined}, scienceObs=${pending.scienceObservationId}"
                            }
              yield (coords, f2Config, duration) match
                case (None, _, _) =>
                  Left(s"Missing target coordinates for science observation ${pending.scienceObservationId}")
                case (_, None, _) =>
                  Left(s"Missing F2 config for science observation ${pending.scienceObservationId}")
                case (_, _, None) =>
                  Left(s"Missing observation duration for science observation ${pending.scienceObservationId}")
                case (Some(c), Some(config), Some(d)) =>
                  Right((c, config.telluricType, d))

        // Create target from sidereal data and update asterism (in transaction)
        def createAndLinkTarget(sidereal: lucuma.core.model.Target.Sidereal): F[Target.Id] =
          S.transactionally:
            Services.asSuperUser:
              val catalogInfoInput = sidereal.catalogInfo.map: ci =>
                CatalogInfoInput(ci.catalog.some, ci.id.some, ci.objectType)
              val targetInput = TargetPropertiesInput.Create(
                name = sidereal.name,
                subtypeInfo = SiderealInput.Create(
                  ra = sidereal.tracking.baseCoordinates.ra,
                  dec = sidereal.tracking.baseCoordinates.dec,
                  epoch = sidereal.tracking.epoch,
                  properMotion = sidereal.tracking.properMotion,
                  radialVelocity = sidereal.tracking.radialVelocity,
                  parallax = sidereal.tracking.parallax,
                  catalogInfo = catalogInfoInput
                ),
                sourceProfile = sidereal.sourceProfile,
                existence = Existence.Present
              )
              for
                // Create the target
                targetId     <- targetService.createTarget(
                                  AccessControl.unchecked(targetInput, pending.programId, program_id),
                                  disposition = TargetDisposition.Calibration,
                                  role = CalibrationRole.Telluric.some
                                ).orError
                // Get current target (may be empty)
                oldTargetOpt <- session.prepareR(Statements.selectTelluricObservationTarget)
                                  .use(_.option(pending.observationId))
                // Insert or update observation target
                // Old targets will be cleaned up by orphan calibration targets cleanup
                _            <- oldTargetOpt match
                                  case Some(_) =>
                                    session.execute(Statements.updateTelluricObservationTarget)(
                                      (targetId, pending.observationId)
                                    )
                                  case None =>
                                    session.execute(Statements.insertTelluricObservationTarget)(
                                      (pending.programId, pending.observationId, targetId)
                                    )
              yield targetId

        // Main resolution logic
        for
          _          <- debug"Resolving telluric target for observation ${pending.observationId}"
          searchData <- querySearchData
          result     <- searchData match
                          case Right((coords, telluricType, duration)) =>
                            val searchInput = TelluricSearchInput(
                              coordinates = coords,
                              duration = duration,
                              brightest = BigDecimal(8.0),
                              spType = telluricType
                            )
                            // HTTP call outside transaction
                            telluricClient.searchTarget(searchInput).flatMap: results =>
                              // Find first successful result
                              results.headOption match
                                case Some((star, catalogResult)) =>
                                  // Use catalog result if available, otherwise convert TelluricStar
                                  val sidereal = catalogResult
                                    .map(_.target)
                                    .getOrElse(toSiderealTarget(star))
                                  info"Found telluric star HIP ${star.hip} for observation ${pending.observationId}" *>
                                  createAndLinkTarget(sidereal).map(Right(_))
                                case None =>
                                  val msg = s"No telluric stars found for observation ${pending.observationId}"
                                  info"$msg".as(Left(msg))
                          case Left(msg) =>
                            info"$msg".as(Left(msg))
          meta       <- result match
                          case Right(targetId) =>
                            updateToReady(targetId.some, none)
                          case Left(errorMsg) if pending.failureCount < 5 =>
                            warn"Telluric resolution failed (attempt ${pending.failureCount + 1}), will retry: $errorMsg" *>
                            updateToRetry(pending.failureCount, errorMsg)
                          case Left(errorMsg) =>
                            error"Telluric resolution permanently failed after ${pending.failureCount} attempts: $errorMsg" *>
                            updateToReady(none, errorMsg.some)
        yield meta

      override def recheckForScienceObservation(
        scienceObsId: Observation.Id
      )(using ServiceAccess, NoTransaction[F]): F[Unit] =

        def extractHip(name: String): Option[Int] =
          name.stripPrefix("HIP ").toIntOption

        def recheckOne(telluricObsId: Observation.Id, programId: Program.Id, currentTargetId: Target.Id): F[Unit] =
          for
            // Get current target name to extract HIP number
            currentName <- S.transactionally:
                             Services.asSuperUser:
                               session.prepareR(Statements.selectTargetName).use(_.unique(currentTargetId))
            currentHip   = extractHip(currentName)

            // Get science observation data for search
            searchResult <- S.transactionally:
                              Services.asSuperUser:
                                for
                                  coords   <- session.prepareR(Statements.selectTargetCoordinates)
                                                .use(_.option(scienceObsId))
                                  f2Config <- flamingos2LongSlitService.select(List(scienceObsId))
                                                .map(_.get(scienceObsId))
                                  duration <- session.prepareR(Statements.selectObservationDuration)
                                                .use(_.option(scienceObsId))
                                yield (coords, f2Config, duration)

            // Perform search and compare
            _ <- searchResult match
                   case (Some(c), Some(config), Some(duration)) =>
                     val searchInput = TelluricSearchInput(
                       coordinates = c,
                       duration = duration,
                       brightest = BigDecimal(8.0),
                       spType = config.telluricType
                     )
                     telluricClient.search(searchInput).flatMap: stars =>
                       stars.headOption match
                         case Some(star) =>
                           if currentHip.contains(star.hip) then
                             info"Telluric star unchanged for $telluricObsId (HIP ${star.hip})"
                           else
                             info"Telluric star changed for $telluricObsId: ${currentName} -> HIP ${star.hip}" *>
                             S.transactionally:
                               Services.asSuperUser:
                                 for
                                   // Create new target
                                   newTargetId <- createNewTelluricTarget(programId, star)
                                   // Update observation to point to new target
                                   // Old target will be cleaned up by orphan calibration targets cleanup
                                   _ <- session.execute(Statements.updateTelluricObservationTarget)(
                                          (newTargetId, telluricObsId)
                                        )
                                   // Update resolution record
                                   _ <- session.execute(
                                          sql"""
                                            UPDATE t_telluric_resolution
                                            SET c_resolved_target_id = $target_id,
                                                c_last_update = now()
                                            WHERE c_observation_id = $observation_id
                                          """.command
                                        )(newTargetId, telluricObsId)
                                 yield ()
                         case None =>
                           warn"No telluric stars found during recheck for $telluricObsId"
                   case _ =>
                     warn"Missing coordinates or F2 config during recheck for $telluricObsId"
          yield ()

        def createNewTelluricTarget(
          pid: Program.Id,
          star: lucuma.catalog.telluric.TelluricStar
        )(using Transaction[F], Services.SuperUserAccess): F[Target.Id] =
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
            AccessControl.unchecked(targetInput, pid, program_id),
            disposition = TargetDisposition.Calibration,
            role = CalibrationRole.Telluric.some
          ).orError

        // Main logic: find all resolved telluric entries for this science observation and recheck them
        // Also reset any failed entries to pending so they can be retried
        for
          resolved <- S.transactionally:
                        Services.asSuperUser:
                          session.prepareR(Statements.selectResolvedForScienceObs)
                            .use(_.stream(scienceObsId, 64).compile.toList)
          _        <- resolved.traverse_(recheckOne.tupled)
          // Reset failed entries (ready state with no resolved target) to pending
          completion <- S.transactionally:
                          Services.asSuperUser:
                            session.execute(Statements.resetFailedForScienceObs)(scienceObsId)
          resetCount = completion match
                         case skunk.data.Completion.Update(n) => n.toInt
                         case _ => 0
          _        <- info"Reset $resetCount failed telluric entries to pending for science observation $scienceObsId".whenA(resetCount > 0)
        yield ()

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
            SELECT t.c_sid_ra, t.c_sid_dec
            FROM   t_asterism_target a
            JOIN   t_target t ON t.c_target_id = a.c_target_id
            WHERE  a.c_observation_id = $observation_id
              AND  t.c_sid_ra IS NOT NULL
              AND  t.c_sid_dec IS NOT NULL
            LIMIT  1
          """.query(right_ascension *: declination).map(Coordinates.apply)

        val selectObservationDuration: Query[Observation.Id, TimeSpan] =
          sql"""
            SELECT COALESCE(c_acq_non_charged_time, '0'::interval) +
                   COALESCE(c_acq_program_time, '0'::interval) +
                   COALESCE(c_sci_non_charged_time, '0'::interval) +
                   COALESCE(c_sci_program_time, '0'::interval) +
                   COALESCE(c_full_setup_time, '0'::interval)
            FROM   t_obscalc
            WHERE  c_observation_id = $observation_id
          """.query(time_span)

        val selectTelluricObservationTarget: Query[Observation.Id, Target.Id] =
          sql"""
            SELECT a.c_target_id
            FROM   t_asterism_target a
            WHERE  a.c_observation_id = $observation_id
            LIMIT  1
          """.query(target_id)

        val updateTelluricObservationTarget: Command[(Target.Id, Observation.Id)] =
          sql"""
            UPDATE t_asterism_target
            SET    c_target_id = $target_id
            WHERE  c_observation_id = $observation_id
          """.command

        val insertTelluricObservationTarget: Command[(Program.Id, Observation.Id, Target.Id)] =
          sql"""
            INSERT INTO t_asterism_target (c_program_id, c_observation_id, c_target_id)
            VALUES ($program_id, $observation_id, $target_id)
          """.command

        val insertResolutionRequest: Command[(Observation.Id, Program.Id, Observation.Id)] =
          sql"""
            INSERT INTO t_telluric_resolution (
              c_observation_id,
              c_program_id,
              c_science_observation_id,
              c_state,
              c_last_invalidation
            ) VALUES (
              $observation_id,
              $program_id,
              $observation_id,
              'pending',
              now()
            )
            ON CONFLICT (c_observation_id) DO NOTHING
          """.command

        val selectResolvedForScienceObs: Query[Observation.Id, (Observation.Id, Program.Id, Target.Id)] =
          sql"""
            SELECT c_observation_id, c_program_id, c_resolved_target_id
            FROM   t_telluric_resolution
            WHERE  c_science_observation_id = $observation_id
              AND  c_state = 'ready'
              AND  c_resolved_target_id IS NOT NULL
          """.query(observation_id *: program_id *: target_id)

        val selectTargetName: Query[Target.Id, String] =
          sql"""
            SELECT c_name
            FROM   t_target
            WHERE  c_target_id = $target_id
          """.query(text)

        val resetFailedForScienceObs: Command[Observation.Id] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'pending',
                   c_last_invalidation = now(),
                   c_retry_at = NULL,
                   c_failure_count = 0,
                   c_error_message = NULL
            WHERE  c_science_observation_id = $observation_id
              AND  c_state = 'ready'
              AND  c_resolved_target_id IS NULL
          """.command
