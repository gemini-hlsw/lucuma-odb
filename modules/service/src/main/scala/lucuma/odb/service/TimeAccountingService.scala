// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*


trait TimeAccountingService[F[_]] {

  /**
   * Extracts the initial time accounting state for the given visit.
   */
  def initialState(
    visitId: Visit.Id
  )(using Transaction[F]): F[TimeAccountingState]

  /**
   * Initializes the time accounting data when a new visit is created.
   */
  def initialize(
    visitId: Visit.Id
  )(using Transaction[F]): F[Unit]

  /**
   * Updates the time accounting data.
   */
  def update(
    visitId: Visit.Id
  )(using Transaction[F]): F[Unit]
}

object TimeAccountingService {

  def instantiate[F[_]: Concurrent](using Services[F]): TimeAccountingService[F] =
    new TimeAccountingService[F] {

      import Statements.*

      override def initialState(
        visitId: Visit.Id
      )(using Transaction[F]): F[TimeAccountingState] =

        for {
          o <- session.unique(SelectObservationId)(visitId)
          c <- session.unique(SelectObserveClass)(o)
          s <- session
                 .stream(SelectEvents)((c, visitId), 1024)
                 .through(TimeAccountingState.eventStreamPipe(c.chargeClass, visitId))
                 .compile
                 .onlyOrError
        } yield s

      override def initialize(
        visitId: Visit.Id
      )(using Transaction[F]): F[Unit] =
        session.execute(SetTimeAccounting)((visitId, CategorizedTime.Zero, CategorizedTime.Zero)).void

      override def update(
        visitId: Visit.Id
      )(using Transaction[F]): F[Unit] =
        for {
          s <- initialState(visitId)
          _ <- session.execute(UpdateTimeAccounting)((visitId, s.charge, s.charge))
        } yield ()

    }


  object Statements {

    val SetTimeAccounting: Command[(Visit.Id, CategorizedTime, CategorizedTime)] =
      sql"""
        INSERT INTO t_time_accounting (
          c_visit_id,
          c_raw_non_charged_time,
          c_raw_partner_time,
          c_raw_program_time,
          c_final_non_charged_time,
          c_final_partner_time,
          c_final_program_time
        ) VALUES (
          $visit_id,
          $categorized_time,
          $categorized_time
        )
      """.command

    val UpdateTimeAccounting: Command[(
      Visit.Id,
      CategorizedTime,
      CategorizedTime
    )] =
      sql"""
        UPDATE t_time_accounting
           SET c_raw_non_charged_time   = $time_span,
               c_raw_partner_time       = $time_span,
               c_raw_program_time       = $time_span,
               c_final_non_charged_time = $time_span,
               c_final_partner_time     = $time_span,
               c_final_program_time     = $time_span
         WHERE c_visit_id = $visit_id
      """.command
         .contramap { case (v, r, f) => (
           r.nonCharged,
           r.partnerTime,
           r.programTime,
           f.nonCharged,
           f.partnerTime,
           f.programTime,
           v
         )}

    val SelectObservationId: Query[Visit.Id, Observation.Id] =
      sql"""
        SELECT
          c_observation_id
        FROM
          t_visit
        WHERE
          c_visit_id = $visit_id
      """.query(observation_id)

    val SelectObserveClass: Query[Observation.Id, ObserveClass] =
      sql"""
        SELECT
          COALESCE(MIN(s.c_observe_class), 'science'::e_obs_class)
        FROM
          t_step_record s
        INNER JOIN t_atom_record a ON
          a.c_atom_id = s.c_atom_id
        WHERE
          a.c_observation_id = $observation_id
      """.query(obs_class)

    private object codec {
      val step_context: Decoder[TimeAccounting.StepContext] =
        (atom_id *: step_id).to[TimeAccounting.StepContext]

      val context: Decoder[TimeAccounting.Context] =
        (
          visit_id                     *:
          obs_class.map(_.chargeClass) *:
          step_context.opt
        ).to[TimeAccounting.Context]

      val event: Decoder[TimeAccounting.Event] =
        (core_timestamp *: context).to[TimeAccounting.Event]
    }

    val SelectEvents: Query[(ObserveClass, Visit.Id), TimeAccounting.Event] =
      sql"""
        SELECT
          e.c_received,
          e.c_visit_id,
          COALESCE(s.c_observe_class, $obs_class),
          s.c_atom_id,
          e.c_step_id
        FROM
          t_execution_event e
        LEFT JOIN t_step_record s ON s.c_step_id = e.c_step_id
        WHERE
          e.c_visit_id = $visit_id
        ORDER BY
          e.c_received
      """.query(codec.event)

  }

}
