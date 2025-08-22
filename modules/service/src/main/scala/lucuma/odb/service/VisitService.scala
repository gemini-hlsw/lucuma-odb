// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Visit
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
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

  def lookupOrInsert(
    observationId: Observation.Id
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordFlamingos2(
    input: RecordVisitInput[Flamingos2StaticConfig]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordGmosNorth(
    input: RecordVisitInput[GmosNorth]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def recordGmosSouth(
    input: RecordVisitInput[GmosSouth]
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]


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

      private def obsDescription(
        observationId: Observation.Id
      ): ResultT[F, ObsDescription] =
        ResultT:
          session.option(Statements.SelectObsDescription)(observationId).map: od =>
            Result.fromOption(od, OdbError.InvalidObservation(observationId, s"Observation '$observationId' not found or is not associated with any instrument.".some).asProblem)


      override def lookupOrInsert(
        observationId: Observation.Id
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =

        def obsNight(site: Site): ResultT[F, ObservingNight] =
          ResultT.liftF(timeService.currentObservingNight(site))

        // Application transaction advisory lock on visit creation. The idea is
        // to prevent two callers (Observe and Navigate) from doing a lookup
        // simultaneously and coming to the conclusion to each create a new
        // visit.
        val lockVisitCreation: ResultT[F, Unit] =
          ResultT.liftF:
            session.unique(Statements.LockCreation)(Statements.VisitCreationLockId)

        def lookupVisit(desc:  ObsDescription, night: ObservingNight): ResultT[F, Option[Visit.Id]] =
          ResultT.liftF:
            if desc.isChargeable then
              session
                .option(Statements.SelectChargeableVisit)(night)
                .map: o =>
                  o.collect:
                    case vr if vr.observationId === observationId => vr.visitId
            else
              session
                .option(Statements.SelectLastVisit)(night, observationId)
                .map(_.map(_.visitId))

        def insertNewVisit(desc: ObsDescription): ResultT[F, Visit.Id] =
           ResultT.liftF:
             session.unique(Statements.InsertVisit)(observationId, desc)

        (for
          d  <- obsDescription(observationId)
          n  <- obsNight(d.site)
          _  <- lockVisitCreation
          v  <- lookupVisit(d, n)
          vʹ <- v.fold(insertNewVisit(d))(ResultT.pure)
        yield vʹ).value

      private def insertWithStaticConfig[A](
        observationId: Observation.Id,
        static:        A,
        instrument:    Instrument,
        lookupStatic:  Visit.Id                              => F[Option[A]],
        insertStatic:  (Observation.Id, Option[Visit.Id], A) => F[Long]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =

        val insertNewVisit: ResultT[F, Visit.Id] =
          for
            d <- obsDescription(observationId)
            v <- ResultT.liftF(session.unique(Statements.InsertVisit)(observationId, d))
          yield v

        def insertStaticForVisit(v: Visit.Id): ResultT[F, Unit] =
          ResultT.liftF(insertStatic(observationId, v.some, static)).void

        val update = (for
          v0 <- ResultT(lookupOrInsert(observationId))
          os <- ResultT.liftF(lookupStatic(v0))
          v1 <- os.fold(insertStaticForVisit(v0).as(v0)): _ =>
                  insertNewVisit.flatMap: v =>
                    insertStaticForVisit(v).as(v)
        yield v1).value

        update
          .recover:
            case SqlState.ForeignKeyViolation(_) =>
              OdbError.InvalidObservation(observationId, Some(s"Observation '$observationId' not found or is not a ${instrument.longName} observation")).asFailure

      def recordFlamingos2(
        input: RecordVisitInput[Flamingos2StaticConfig]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        insertWithStaticConfig(
          input.observationId,
          input.static,
          Instrument.Flamingos2,
          flamingos2SequenceService.selectStatic,
          flamingos2SequenceService.insertStatic
        )

      override def recordGmosNorth(
        input: RecordVisitInput[GmosNorth]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        insertWithStaticConfig(
          input.observationId,
          input.static,
          Instrument.GmosNorth,
          gmosSequenceService.selectGmosNorthStatic,
          gmosSequenceService.insertGmosNorthStatic
        )

      override def recordGmosSouth(
        input: RecordVisitInput[GmosSouth]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        insertWithStaticConfig(
          input.observationId,
          input.static,
          Instrument.GmosSouth,
          gmosSequenceService.selectGmosSouthStatic,
          gmosSequenceService.insertGmosSouthStatic
        )

  object Statements:

    private val obs_description: Codec[ObsDescription] =
      (instrument *: calibration_role.opt).to[ObsDescription]

    private val visit_record: Codec[VisitRecord] =
      (visit_id *: observation_id *: instrument *: core_timestamp *: lucuma.odb.util.Codecs.site *: bool).to[VisitRecord]

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
          c_chargeable
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
          c_chargeable
        FROM t_visit
        WHERE c_observation_id = $observation_id
      """.query(visit_record)

    val SelectChargeableVisit: Query[ObservingNight, VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created,
          c_site,
          c_chargeable
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
          c_chargeable
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

    val InsertVisit: Query[(Observation.Id, ObsDescription), Visit.Id] =
      sql"""
        INSERT INTO t_visit (
          c_observation_id,
          c_instrument,
          c_site,
          c_chargeable
        )
        SELECT
          $observation_id,
          $instrument,
          ${lucuma.odb.util.Codecs.site},
          $bool
        RETURNING
          c_visit_id
      """.query(visit_id).contramap: (oid, od) =>
        (
          oid,
          od.instrument,
          od.site,
          od.isChargeable
        )
