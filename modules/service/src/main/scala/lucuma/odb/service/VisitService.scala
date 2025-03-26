// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.sequence.data.VisitRecord
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.{site as _, *}
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.temporal.timestamp
import skunk.codec.temporal.timestamptz
import skunk.implicits.*

import java.time.LocalDateTime
import java.time.ZoneOffset.UTC

import Services.Syntax.*

trait VisitService[F[_]]:

  def select(
    visitId: Visit.Id
  ): F[Option[VisitRecord]]

  def selectAll(
    observationId: Observation.Id
  ): Stream[F, VisitRecord]

  def insertGmosNorth(
    observationId: Observation.Id,
    static:        GmosNorth
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def insertGmosSouth(
    observationId: Observation.Id,
    static:        GmosSouth
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def lookupOrCreate(
    observationId: Observation.Id
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
      ): F[Option[VisitRecord]] =
        session.option(Statements.SelectVisit)(visitId)

      override def selectAll(
        observationId: Observation.Id
      ): Stream[F, VisitRecord] =
        session.stream(VisitService.Statements.SelectAllVisit)(observationId, 1024)

      private def obsDescription(
        observationId: Observation.Id
      )(using Transaction[F]): F[Result[ObsDescription]] =
        session.option(Statements.SelectObsDescription)(observationId).map: od =>
          Result.fromOption(od, OdbError.InvalidObservation(observationId, s"Observation '$observationId' not found.".some).asProblem)

      private def insertVisit(
        oid:  Observation.Id,
        inst: Instrument,
        desc: ObsDescription
      ): F[Result[Visit.Id]] =
        session
          .unique(Statements.InsertVisit)(oid, desc)
          .map(Result.success)
          .recover:
            case SqlState.ForeignKeyViolation(_) =>
              OdbError.InvalidObservation(oid, Some(s"Observation '$oid' not found or is not a ${inst.longName} observation")).asFailure

      override def lookupOrCreate(
        observationId: Observation.Id
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =

        def lookupVisit(od: ObsDescription, night: ObservingNight): F[Option[Visit.Id]] =
          if od.isChargeable then
            session
              .option(Statements.SelectChargeableVisit)(night)
              .map: o =>
                o.collect {
                  case vr if vr.observationId === observationId => vr.visitId
                }
          else
            session
              .option(Statements.SelectLastVisit)(night, observationId)
              .map(_.map(_.visitId))

        (for
          od <- ResultT(obsDescription(observationId))
          n  <- ResultT.liftF(session.unique(Statements.selectObservingNight(od.site)))
          v  <- ResultT.liftF(lookupVisit(od, n))
          v2 <- v.fold(ResultT(insertVisit(observationId, od.instrument, od)))(ResultT.pure)
        yield v2).value

      private def insert(
        observationId: Observation.Id,
        instrument:    Instrument,
        insertStatic:  Option[Visit.Id] => F[Long]
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        (for
          o <- ResultT(obsDescription(observationId))
          v <- ResultT(insertVisit(observationId, instrument, o))
          _ <- ResultT.liftF(insertStatic(v.some).void)
        yield v).value

      override def insertGmosNorth(
        observationId: Observation.Id,
        static:        GmosNorth
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        insert(observationId, Instrument.GmosNorth, gmosSequenceService.insertGmosNorthStatic(observationId, _, static))

      override def insertGmosSouth(
        observationId: Observation.Id,
        static:        GmosSouth
      )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]] =
        insert(observationId, Instrument.GmosSouth, gmosSequenceService.insertGmosSouthStatic(observationId, _, static))

  object Statements:

    private val obs_description: Codec[ObsDescription] =
      (instrument *: calibration_role.opt).to[ObsDescription]

    private val visit_record: Codec[VisitRecord] =
      (visit_id *: observation_id *: instrument *: core_timestamp *: lucuma.odb.util.Codecs.site *: bool).to[VisitRecord]

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

// Something like this might work for an all-database solution to the night
// start calculation. I decided to use the Scala calculation though to avoid
// any future inconsistencies.
//    val SelectNightStart: Query[ZoneId, Timestamp] =
//      sql"""
//        SELECT (
//            DATE_TRUNC('day', NOW() AT TIME ZONE $text) + INTERVAL '14 hours'
//            - INTERVAL '1 day' * (NOW() AT TIME ZONE $text < DATE_TRUNC('day', NOW() AT TIME ZONE $text) + INTERVAL '14 hours')::int
//        )::timestamptz
//      """.query(core_timestamp).contramap(z => (z.getId, z.getId, z.getId))

    def selectObservingNight(site: Site): Query[Void, ObservingNight] =
      sql"""SELECT NOW()"""
        .query(timestamptz)
        .map(t => ObservingNight.fromSiteAndInstant(site, t.toInstant))

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