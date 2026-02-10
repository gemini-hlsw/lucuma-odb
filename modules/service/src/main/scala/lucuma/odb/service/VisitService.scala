// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Visit
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.core.util.IdempotencyKey
import lucuma.odb.data.OdbError
import lucuma.odb.data.ResultExtensions.*
import lucuma.odb.graphql.input.RecordVisitInput
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.{site as _, *}
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8
import skunk.codec.temporal.timestamp
import skunk.implicits.*

import java.time.LocalDateTime
import java.time.ZoneOffset.UTC

import Services.Syntax.*

trait VisitService[F[_]]:

  def select(
    visitId: Visit.Id
  )(using Services.ServiceAccess): F[Option[VisitRecord]]

  def selectAll(
    observationId: Observation.Id
  )(using Services.ServiceAccess): Stream[F, VisitRecord]

  def hasVisits(
    observationId: Observation.Id
  )(using Transaction[F]): F[Boolean]

  def lookupOrInsert(
    observationId:  Observation.Id,
    idempotencyKey: Option[IdempotencyKey]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordFlamingos2(
    input: RecordVisitInput[Flamingos2StaticConfig]
  )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordGmosNorth(
    input: RecordVisitInput[GmosNorth]
  )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordGmosSouth(
    input: RecordVisitInput[GmosSouth]
  )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]]


object VisitService:

  case class ObsDescription(
    instrument:      Instrument,
    calibrationRole: Option[CalibrationRole]
  ):
    // TODO: need to make instrument and site 1:1
    def site: Site =
      instrument.site.head

    // Determine whether an observation is chargeable.  I'd originally planned
    // to do this by generating the sequence and dropping atoms until the first
    // one with a chargeable obsclass was found (if ever).  That's a more
    // general way that wouldn't need to be updated as code changes, but this is
    // far cheaper and is correct at least for now.
    def isChargeable: Boolean =
      calibrationRole match
        case None                                     |
             Some(CalibrationRole.Photometric)        |
             Some(CalibrationRole.SpectroPhotometric) |
             Some(CalibrationRole.Telluric)            => true
        case Some(CalibrationRole.Twilight)            => false

  def instantiate[F[_]: Concurrent](using Services[F]): VisitService[F] =
    new VisitService[F]:

      override def select(
        visitId: Visit.Id
      )(using Services.ServiceAccess): F[Option[VisitRecord]] =
        session.option(Statements.SelectVisit)(visitId)

      override def selectAll(
        observationId: Observation.Id
      )(using Services.ServiceAccess): Stream[F, VisitRecord] =
        session.stream(VisitService.Statements.SelectAllVisit)(observationId, 1024)

      override def hasVisits(
        observationId: Observation.Id
      )(using Transaction[F]): F[Boolean] =
        session.unique(Statements.HasVisits)(observationId)

      private def obsDescription(
        observationId: Observation.Id
      ): EitherT[F, OdbError, ObsDescription] =
        EitherT:
          session.option(Statements.SelectObsDescription)(observationId).map: od =>
            Either.fromOption(od, OdbError.InvalidObservation(observationId, s"Observation '$observationId' not found or is not associated with any instrument.".some))

      private def lookupOrInsertImpl(
        observationId:  Observation.Id,
        idempotencyKey: Option[IdempotencyKey]
      )(using Transaction[F], Services.ServiceAccess): EitherT[F, OdbError, VisitRecord] =

        def lookupVisit(desc:  ObsDescription, night: ObservingNight): F[Option[VisitRecord]] =
          if desc.isChargeable then
            session
              .option(Statements.SelectChargeableVisit)(night)
              .map: o =>
                o.collect:
                  case vr if vr.observationId === observationId => vr
          else
            session
              .option(Statements.SelectLastVisit)(night, observationId)

        def insertNewVisit(desc: ObsDescription): F[VisitRecord] =
          session.unique(Statements.InsertVisit)(observationId, desc, idempotencyKey)

        obsDescription(observationId)
          .semiflatMap: d =>
            for
              n  <- timeService.currentObservingNight(d.site)
              // Application transaction advisory lock on visit creation. The idea is
              // to prevent two callers (Observe and Navigate) from doing a lookup
              // simultaneously and coming to the conclusion to each create a new
              // visit.
              _  <- session.unique(Statements.LockCreation)(Statements.VisitCreationLockId)
              v  <- lookupVisit(d, n)
              vʹ <- v.fold(insertNewVisit(d))(Concurrent[F].pure)
            yield vʹ

      override def lookupOrInsert(
        observationId:  Observation.Id,
        idempotencyKey: Option[IdempotencyKey]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        lookupOrInsertImpl(observationId, idempotencyKey)
          .map(_.visitId)
          .value
          .map(Result.fromEitherOdbError)

      private def insertWithStaticConfig[A](
        observationId:  Observation.Id,
        static:         A,
        idempotencyKey: Option[IdempotencyKey],
        instrument:     Instrument,
        lookupStatic:   Visit.Id => Transaction[F] ?=> F[Option[A]],
        insertStatic:   (Observation.Id, Option[Visit.Id], A) => (Transaction[F], Services.ServiceAccess) ?=> F[Long]
      )(using Transaction[F], Services.ServiceAccess): F[Either[OdbError, Visit.Id]] =

        val insertNewVisit: EitherT[F, OdbError, Visit.Id] =
          for
            d <- obsDescription(observationId)
            v <- EitherT.liftF(session.unique(Statements.InsertVisit)(observationId, d, idempotencyKey))
          yield v.visitId

        def insertStaticForVisit(v: Visit.Id): EitherT[F, OdbError, Unit] =
          EitherT.liftF(insertStatic(observationId, v.some, static).void)

        val update = for
          v0 <- lookupOrInsertImpl(observationId, idempotencyKey)
          os <- EitherT.liftF(lookupStatic(v0.visitId))
          v1 <- os.fold(insertStaticForVisit(v0.visitId).as(v0.visitId)): _ =>
                  val sameKey = (v0.idempotencyKey, idempotencyKey).tupled.exists(_ === _)
                  if sameKey then
                    EitherT.pure[F, OdbError](v0.visitId)
                  else
                    insertNewVisit.flatMap: v =>
                      insertStaticForVisit(v).as(v)
        yield v1

        update
          .value
          .recover:
            case SqlState.ForeignKeyViolation(_) =>
              OdbError.InvalidObservation(observationId, s"Observation '$observationId' not found or is not a ${instrument.longName} observation".some).asLeft[Visit.Id]

      private def materializeAndRecord[A](
        observationId:  Observation.Id,
        static:         A,
        idempotencyKey: Option[IdempotencyKey],
        instrument:     Instrument,
        lookupStatic:   Visit.Id => Transaction[F] ?=> F[Option[A]],
        insertStatic:   (Observation.Id, Option[Visit.Id], A) => (Transaction[F], Services.ServiceAccess) ?=> F[Long]
      )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        generator
          .materializeAndThen(observationId):
            insertWithStaticConfig(observationId, static, idempotencyKey, instrument, lookupStatic, insertStatic)
          .map(Result.fromEitherOdbError)

      def recordFlamingos2(
        input: RecordVisitInput[Flamingos2StaticConfig]
      )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        materializeAndRecord(
          input.observationId,
          input.static,
          input.idempotencyKey,
          Instrument.Flamingos2,
          flamingos2SequenceService.selectStatic,
          flamingos2SequenceService.insertStatic
        )

      override def recordGmosNorth(
        input: RecordVisitInput[GmosNorth]
      )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        materializeAndRecord(
          input.observationId,
          input.static,
          input.idempotencyKey,
          Instrument.GmosNorth,
          gmosSequenceService.selectGmosNorthStatic,
          gmosSequenceService.insertGmosNorthStatic
        )

      override def recordGmosSouth(
        input: RecordVisitInput[GmosSouth]
      )(using NoTransaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        materializeAndRecord(
          input.observationId,
          input.static,
          input.idempotencyKey,
          Instrument.GmosSouth,
          gmosSequenceService.selectGmosSouthStatic,
          gmosSequenceService.insertGmosSouthStatic
        )

  object Statements:

    private val obs_description: Codec[ObsDescription] =
      (instrument *: calibration_role.opt).to[ObsDescription]

    private val visit_record: Codec[VisitRecord] =
      (
        visit_id                    *:
        observation_id              *:
        instrument                  *:
        core_timestamp              *:
        lucuma.odb.util.Codecs.site *:
        bool                        *:
        idempotency_key.opt
      ).to[VisitRecord]

    val VisitCreationLockId = 100l

    val LockCreation: Query[Long, Unit] =
      sql"""
        SELECT pg_advisory_xact_lock($int8)
      """.query(void)

    val SelectVisit: Query[Visit.Id, VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable,
          c_idempotency_key
        FROM t_visit
        WHERE c_visit_id = $visit_id
      """.query(visit_record)

    val SelectAllVisit: Query[Observation.Id, VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable,
          c_idempotency_key
        FROM t_visit
        WHERE c_observation_id = $observation_id
      """.query(visit_record)

    val HasVisits: Query[Observation.Id, Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
          FROM t_visit
          WHERE c_observation_id = $observation_id
        )
      """.query(bool)

    val SelectChargeableVisit: Query[ObservingNight, VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable,
          c_idempotency_key
         FROM t_visit
        WHERE c_chargeable = true
          AND c_created >= $timestamp
          AND c_created  < $timestamp
          AND c_site = ${lucuma.odb.util.Codecs.site}
        ORDER BY c_created DESC LIMIT 1;
      """
        .query(visit_record)
        .contramap: n =>
          (
            LocalDateTime.ofInstant(n.start, UTC),
            LocalDateTime.ofInstant(n.end,   UTC),
            n.site
          )

    val SelectLastVisit: Query[(ObservingNight, Observation.Id), VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable,
          c_idempotency_key
         FROM t_visit
        WHERE c_created >= $timestamp
          AND c_created  < $timestamp
          AND c_observtation_id = $observation_id
        ORDER BY c_created DESC LIMIT 1;
      """
        .query(visit_record)
        .contramap: (n, o) =>
          (
            LocalDateTime.ofInstant(n.start, UTC),
            LocalDateTime.ofInstant(n.end,   UTC),
            o
          )

    val SelectObsDescription: Query[Observation.Id, ObsDescription] =
      sql"""
        SELECT c_instrument,
               c_calibration_role
          FROM t_observation
         WHERE c_observation_id=$observation_id
           AND c_instrument IS NOT NULL
      """.query(obs_description)

    val InsertVisit: Query[(Observation.Id, ObsDescription, Option[IdempotencyKey]), VisitRecord] =
      sql"""
        INSERT INTO t_visit (
          c_observation_id,
          c_idempotency_key,
          c_instrument,
          c_site,
          c_chargeable
        )
        SELECT
          $observation_id,
          ${idempotency_key.opt},
          $instrument,
          ${lucuma.odb.util.Codecs.site},
          $bool
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable,
          c_idempotency_key
      """.query(visit_record).contramap: (oid, od, idm) =>
        (
          oid,
          idm,
          od.instrument,
          od.site,
          od.isChargeable
        )
