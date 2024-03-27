// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait VisitService[F[_]] {

  def select(
    visitId: Visit.Id
  ): F[Option[VisitService.VisitRecord]]

  def insertGmosNorth(
    observationId: Observation.Id,
    static:        GmosNorth
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

  def insertGmosSouth(
    observationId: Observation.Id,
    static:        GmosSouth
  )(using Transaction[F], Services.ServiceAccess): F[Result[Visit.Id]]

}

object VisitService {

  case class VisitRecord(
    visitId:       Visit.Id,
    observationId: Observation.Id,
    instrument:    Instrument,
    created:       Timestamp
  )

  def instantiate[F[_]: Concurrent](using Services[F]): VisitService[F] =
    new VisitService[F] {

      override def select(
        visitId: Visit.Id
      ): F[Option[VisitService.VisitRecord]] =
        session.option(Statements.SelectVisit)(visitId)

      private def insert(
        observationId: Observation.Id,
        instrument:    Instrument,
        insertStatic:  Option[Visit.Id] => F[Long]
      )(using Services.ServiceAccess): F[Result[Visit.Id]] = {

        val insertVisit: F[Result[Visit.Id]] =
          session
            .unique(Statements.InsertVisit)(observationId, instrument)
            .map(Result.success)
            .recover:
              case SqlState.ForeignKeyViolation(_) =>
                OdbError.InvalidObservation(observationId, Some(s"Observation '$observationId' not found or is not a ${instrument.longName} observation")).asFailure
 
        val rt = for 
          v <- ResultT(insertVisit)
          _ <- ResultT.success(insertStatic(v.some).void)
        yield v

        rt.value

      }

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

    }

  object Statements {

    private val visit_record: Codec[VisitRecord] =
      (visit_id *: observation_id *: instrument *: core_timestamp).to[VisitRecord]

    val SelectVisit: Query[Visit.Id, VisitRecord] =
      sql"""
        SELECT
          c_visit_id,
          c_observation_id,
          c_instrument,
          c_created
        FROM t_visit
        WHERE c_visit_id = $visit_id
      """.query(visit_record)

    val InsertVisit: Query[(Observation.Id, Instrument), Visit.Id] =
      sql"""
        INSERT INTO t_visit (
          c_observation_id,
          c_instrument
        )
        SELECT
          $observation_id,
          $instrument
        RETURNING
          c_visit_id
      """.query(visit_id)

  }
}
