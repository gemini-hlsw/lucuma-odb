// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.effect.Temporal
import cats.syntax.all.*
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.TelluricTargets
import lucuma.odb.graphql.input.CatalogInfoInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.flamingos2.longslit.Config as F2Config
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Codecs.calculation_state
import lucuma.odb.util.Flamingos2Codecs.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import scala.concurrent.duration.*

trait TelluricTargetsService[F[_]]:

  /**
   * Marks all 'calculating' entries as 'pending' on startup.
   * Called by the daemon to clean up state after restart.
   */
  def reset(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * Loads up to `max` pending/retry resolution entries.
   * Changes state to 'calculating' before returning.
   */
  def load(max: Int): F[List[TelluricTargets.Pending]]

  /**
   * Loads a specific pending resolution entry.
   * Changes state to 'calculating' before returning.
   */
  def loadObs(
    oid: Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Option[TelluricTargets.Pending]]

  /**
   * Records a new telluric resolution request.
   * Called when a telluric observation is created.
   */
  def requestTelluricTarget(
    pid:             Program.Id,
    telluricId:      Observation.Id,
    scienceId:       Observation.Id,
    scienceDuration: TimeSpan
  )(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * Resolves the telluric target and updates the database.
   */
  def resolveTargets(
    pending: TelluricTargets.Pending
  )(using ServiceAccess, NoTransaction[F]): F[Option[TelluricTargets.Meta]]

case class HminBrightnessKey(
  disperser: Flamingos2Disperser,
  filter:    Flamingos2Filter,
  fpu:       Flamingos2Fpu
)

object HminBrightnessCache extends NewType[Map[HminBrightnessKey, (Option[BigDecimal], Option[BigDecimal])]]:
  val Empty: HminBrightnessCache = HminBrightnessCache(Map.empty)
  val DefaultHmin: BigDecimal = BigDecimal(8)

  extension(m: HminBrightnessCache)
    def lookup(config: F2Config): BigDecimal =
      val key = HminBrightnessKey(config.disperser, config.filter, config.fpu)
      m.value.get(key).flatMap: (hminHot, hminSolar) =>
        config.telluricType match
          case TelluricType.Solar     => hminSolar
          case TelluricType.Hot       => hminHot
          case TelluricType.A0V       => hminHot
          case _: TelluricType.Manual => hminHot
      .getOrElse(HminBrightnessCache.DefaultHmin)
end HminBrightnessCache

type HminBrightnessCache = HminBrightnessCache.Type

object TelluricTargetsService:

  given HashBytes[TelluricSearchInput] with
    def hashBytes(input: TelluricSearchInput): Array[Byte] =
      Array.concat(
        input.coordinates.hashBytes,
        input.duration.hashBytes,
        input.brightest.hashBytes,
        input.spType.hashBytes
      )

  def loadBrightnessCache[F[_]: Monad](session: Session[F]): F[HminBrightnessCache] =
    session.execute(Statements.SelectAllHmin).map: rows =>
      HminBrightnessCache(rows.map { case (disperser, filter, fpu, hminHot, hminSolar) =>
        HminBrightnessKey(disperser, filter, fpu) -> (hminHot, hminSolar)
      }.toMap)

  def instantiate[F[_]: {Temporal, LoggerFactory as LF, Services as S}](
    telluricClient: TelluricTargetsClient[F],
    hminCache: HminBrightnessCache
  ): TelluricTargetsService[F] =
    new TelluricTargetsService[F]:
      given Logger[F] = LF.getLoggerFromName("telluric-targets")

      val F2MaxDuration = TimeSpan.fromHours(3).get

      private def fetchSearchParams(scienceObsId: Observation.Id)(
        using ServiceAccess): F[Option[(Coordinates, F2Config)]] =
        S.transactionally:
          Services.asSuperUser:
            for
              coords   <- session.prepareR(Statements.SelectTargetCoordinates)
                            .use(_.option(scienceObsId))
              f2Config <- flamingos2LongSlitService.select(List(scienceObsId))
                            .map(_.get(scienceObsId))
            yield (coords, f2Config).tupled

      // Exponential backoff calculation
      // Should this be configurable?
      private def calculateRetryAt(failureCount: Int): FiniteDuration =
        val baseDelay = 30.seconds
        val maxDelay = 1.hour
        val delay = baseDelay * math.pow(2, failureCount.toDouble).toLong
        FiniteDuration(math.min(delay.toSeconds, maxDelay.toSeconds), SECONDS)

      override def reset(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.ResetCalculating).void

      override def load(max: Int): F[List[TelluricTargets.Pending]] =
        session
          .prepareR(Statements.LoadPending)
          .use(_.stream(max, 1024).compile.toList)

      override def loadObs(
        oid: Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Option[TelluricTargets.Pending]] =
        session
          .prepareR(Statements.LoadPendingObs)
          .use(_.option(oid))

      private def mkSearchInput(
        coords:     Coordinates,
        config:     F2Config,
        brightness: BigDecimal,
        duration:   TimeSpan
      ) =
        TelluricSearchInput(
          coordinates = coords,
          duration    = duration,
          brightest   = brightness,
          spType      = config.telluricType
        )

      override def requestTelluricTarget(
        pid:              Program.Id,
        telluricId:      Observation.Id,
        scienceId:       Observation.Id,
        scienceDuration: TimeSpan
      )(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.InsertResolutionRequest)(telluricId, pid, scienceId, scienceDuration).void

      override def resolveTargets(
        pending: TelluricTargets.Pending
      )(using ServiceAccess, NoTransaction[F]): F[Option[TelluricTargets.Meta]] = {

        def requestRetry(
          targetId:   Option[Target.Id],
          errorMsg:   Option[String],
          paramsHash: Option[Md5Hash]
        ): F[Option[TelluricTargets.Meta]] =
          S.transactionally:
            session
              .prepareR(Statements.RequestReady)
              .use(_.option((targetId, errorMsg, paramsHash, pending.observationId, pending.lastInvalidation)))

        def retryRequest(
          failureCount: Int,
          errorMsg: String
        ): F[Option[TelluricTargets.Meta]] =
          val retryDelay = calculateRetryAt(failureCount)
          S.transactionally:
            session
              .prepareR(Statements.RetryRequest)
              .use(_.option((failureCount + 1, s"${retryDelay.toSeconds} seconds", errorMsg, pending.observationId)))

        def queryParams: F[Either[String, (Coordinates, F2Config)]] =
          fetchSearchParams(pending.scienceObservationId)
            .map(_.toRight(s"Missing coordinates or F2 config for science observation ${pending.scienceObservationId}"))

        def createAndLinkTarget(sidereal: Target.Sidereal): F[Target.Id] =
          S.transactionally:
            Services.asSuperUser:
              val catalog = sidereal.catalogInfo.map: ci =>
                CatalogInfoInput(ci.catalog.some, ci.id.some, ci.objectType)
              val target = TargetPropertiesInput.Create(
                name = sidereal.name,
                subtypeInfo = SiderealInput.Create(
                  ra = sidereal.tracking.baseCoordinates.ra,
                  dec = sidereal.tracking.baseCoordinates.dec,
                  epoch = sidereal.tracking.epoch,
                  properMotion = sidereal.tracking.properMotion,
                  radialVelocity = sidereal.tracking.radialVelocity,
                  parallax = sidereal.tracking.parallax,
                  catalogInfo = catalog
                ),
                sourceProfile = sidereal.sourceProfile,
                existence = Existence.Present
              )

              for {
                targetId     <- targetService.createTarget(
                                  AccessControl.unchecked(target, pending.programId, program_id),
                                  disposition = TargetDisposition.Calibration,
                                  role = CalibrationRole.Telluric.some
                                ).orError
                // Get current target
                oldTargetOpt <- session.prepareR(Statements.SelectTelluricObservationTarget)
                                  .use(_.option(pending.observationId))
                // Old targets will be cleaned up by orphan calibration
                _            <- oldTargetOpt match
                                  case Some(_) =>
                                    session.execute(Statements.UpdateTelluricObservationTarget)(
                                      (targetId, pending.observationId)
                                    )
                                  case None =>
                                    session.execute(Statements.InsertTelluricObservationTarget)(
                                      (pending.programId, pending.observationId, targetId)
                                    )
              } yield targetId

        extension (target: Target.Sidereal)
          def sedFromTelluricType(telluricType: TelluricType): Target.Sidereal =
            val sed = telluricType match
              case TelluricType.Solar => UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G2V)
              case _                  => UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V)
            Target.Sidereal.unnormalizedSED.modify(_.orElse(sed.some))(target)

        def fetchExistingTargetId: F[Option[Target.Id]] =
          S.transactionally:
            session
              .prepareR(Statements.SelectResolvedTarget)
              .use(_.option(pending.observationId))

        def searchAndResolve(coords: Coordinates, config: F2Config): F[(Either[String, Target.Id], Md5Hash)] =
          val brightness = hminCache.lookup(config)
          val searchInput = mkSearchInput(coords, config, brightness, pending.scienceDuration.min(F2MaxDuration))
          val paramsHash = Md5Hash.unsafeFromByteArray(searchInput.md5)

          pending.paramsHash match
            case Some(storedHash) if storedHash === paramsHash =>
              info"Hash unchanged for ${pending.observationId}, skipping re-request" *>
                fetchExistingTargetId.map:
                  case Some(tid) => (tid.asRight, paramsHash)
                  case None      => ("No existing target found".asLeft, paramsHash)
            case _ =>
              telluricClient.searchTarget(searchInput).flatMap:
                case (star, catalogResult) :: _ =>
                  val sidereal =
                    catalogResult.map(_.target).getOrElse(star.asSiderealTarget).sedFromTelluricType(config.telluricType)

                  info"Found telluric star HIP ${star.hip} for observation ${pending.observationId}" *>
                    createAndLinkTarget(sidereal).map(tid => (tid.asRight, paramsHash))
                case Nil =>
                  val msg = s"No telluric stars found for observation ${pending.observationId}"
                  Logger[F].warn(msg).as((msg.asLeft, paramsHash))

        def handleResult(result: Either[String, Target.Id], paramsHash: Option[Md5Hash]): F[Option[TelluricTargets.Meta]] =
          result match
            case Right(targetId)                            =>
              requestRetry(targetId.some, none, paramsHash)

            case Left(errorMsg) if pending.failureCount < 5 =>
              warn"Telluric target resolution failed (attempt ${pending.failureCount + 1}), will retry: $errorMsg" *>
                retryRequest(pending.failureCount, errorMsg)

            case Left(errorMsg)                             =>
              error"Telluric target resolution permanently failed after ${pending.failureCount} attempts: $errorMsg" *>
                requestRetry(none, errorMsg.some, paramsHash)

        for {
          _                    <- info"Resolving telluric target for observation ${pending.observationId}"
          searchData           <- queryParams
          (result, paramsHash) <- searchData.fold(
                                    msg => error"$msg".as((msg.asLeft[Target.Id], none[Md5Hash])),
                                    searchAndResolve.tupled.andThen(_.map { case (r, h) => (r, h.some) })
                                  )
          meta                 <- handleResult(result, paramsHash)
        } yield meta
      }

      object Statements:

        val pending: Codec[TelluricTargets.Pending] =
          (observation_id *: program_id *: observation_id *: core_timestamp *: int4 *: time_span *: md5_hash.opt)
            .to[TelluricTargets.Pending]

        val meta: Codec[TelluricTargets.Meta] =
          (observation_id *: program_id *: observation_id *: calculation_state *:
           core_timestamp *: core_timestamp *: core_timestamp.opt *: int4 *:
           target_id.opt *: text.opt *: time_span).to[TelluricTargets.Meta]

        private val pendingColumns: String =
          "c_observation_id, c_program_id, c_science_observation_id, c_last_invalidation, c_failure_count, c_science_duration, c_params_hash"

        private val metaColumns: String =
          """c_observation_id, c_program_id, c_science_observation_id, c_state,
             c_last_invalidation, c_last_update, c_retry_at, c_failure_count,
             c_resolved_target_id, c_error_message, c_science_duration"""

        val ResetCalculating: Command[Void] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'pending',
                   c_retry_at = NULL,
                   c_failure_count = 0
            WHERE  c_state = 'calculating'
          """.command

        val LoadPending: Query[Int, TelluricTargets.Pending] =
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
            RETURNING #$pendingColumns
          """.query(pending)

        val LoadPendingObs: Query[Observation.Id, TelluricTargets.Pending] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'calculating'
            WHERE  c_observation_id = $observation_id
              AND  c_state IN ('pending', 'retry')
              AND  (c_retry_at IS NULL OR c_retry_at <= now())
            RETURNING #$pendingColumns
          """.query(pending)

        val RequestReady: Query[(Option[Target.Id], Option[String], Option[Md5Hash], Observation.Id, Timestamp), TelluricTargets.Meta] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'ready',
                   c_last_update = now(),
                   c_resolved_target_id = ${target_id.opt},
                   c_error_message = ${text.opt},
                   c_params_hash = ${md5_hash.opt},
                   c_retry_at = NULL,
                   c_failure_count = 0
            WHERE  c_observation_id = $observation_id
              AND  c_last_invalidation = $core_timestamp
            RETURNING #$metaColumns
          """.query(meta)

        val RetryRequest: Query[(Int, String, String, Observation.Id), TelluricTargets.Meta] =
          sql"""
            UPDATE t_telluric_resolution
            SET    c_state = 'retry',
                   c_last_update = now(),
                   c_failure_count = $int4,
                   c_retry_at = now() + $text::interval,
                   c_error_message = $text
            WHERE  c_observation_id = $observation_id
            RETURNING #$metaColumns
          """.query(meta)

        val SelectTargetCoordinates: Query[Observation.Id, Coordinates] =
          sql"""
            SELECT t.c_sid_ra, t.c_sid_dec
            FROM   t_asterism_target a
            JOIN   t_target t ON t.c_target_id = a.c_target_id
            WHERE  a.c_observation_id = $observation_id
              AND  t.c_sid_ra IS NOT NULL
              AND  t.c_sid_dec IS NOT NULL
            LIMIT  1
          """.query(right_ascension *: declination).map(Coordinates.apply)

        val SelectTelluricObservationTarget: Query[Observation.Id, Target.Id] =
          sql"""
            SELECT a.c_target_id
            FROM   t_asterism_target a
            WHERE  a.c_observation_id = $observation_id
            LIMIT  1
          """.query(target_id)

        val UpdateTelluricObservationTarget: Command[(Target.Id, Observation.Id)] =
          sql"""
            UPDATE t_asterism_target
            SET    c_target_id = $target_id
            WHERE  c_observation_id = $observation_id
          """.command

        val InsertTelluricObservationTarget: Command[(Program.Id, Observation.Id, Target.Id)] =
          sql"""
            INSERT INTO t_asterism_target (c_program_id, c_observation_id, c_target_id)
            VALUES ($program_id, $observation_id, $target_id)
          """.command

        val InsertResolutionRequest: Command[(Observation.Id, Program.Id, Observation.Id, TimeSpan)] =
          sql"""
            INSERT INTO t_telluric_resolution (
              c_observation_id,
              c_program_id,
              c_science_observation_id,
              c_science_duration,
              c_state,
              c_last_invalidation
            ) VALUES (
              $observation_id,
              $program_id,
              $observation_id,
              $time_span,
              'pending',
              now()
            )
            ON CONFLICT (c_observation_id) DO NOTHING
          """.command

        val SelectResolvedTarget: Query[Observation.Id, Target.Id] =
          sql"""
            SELECT c_resolved_target_id
            FROM   t_telluric_resolution
            WHERE  c_observation_id = $observation_id
              AND  c_resolved_target_id IS NOT NULL
          """.query(target_id)

  object Statements:
    // lead the limits for all the f2 configurations
    val SelectAllHmin: Query[Void, (Flamingos2Disperser, Flamingos2Filter, Flamingos2Fpu, Option[BigDecimal], Option[BigDecimal])] =
      sql"""
        SELECT f2config.c_disperser, f2config.c_filter, f2config.c_fpu,
               config.c_hmin_hot, config.c_hmin_solar
        FROM   t_spectroscopy_config_option_f2 f2config
        JOIN   t_spectroscopy_config_option config
          ON   config.c_instrument = f2config.c_instrument AND config.c_index = f2config.c_index
        WHERE  config.c_instrument = 'Flamingos2'
      """.query(flamingos_2_disperser *: flamingos_2_filter *: flamingos_2_fpu *: numeric.opt *: numeric.opt)
