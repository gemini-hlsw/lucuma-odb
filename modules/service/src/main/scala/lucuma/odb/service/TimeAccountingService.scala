// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.apply.*
import cats.syntax.functor.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.ChargeClass
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Visit
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*


trait TimeAccountingService[F[_]] {

  /**
   */
  def initialState(
    visitId: Visit.Id
  )(using Transaction[F]): F[TimeAccountingState]

}

object TimeAccountingService {

  def instantiate[F[_]: Concurrent](using Services[F]): TimeAccountingService[F] =
    new TimeAccountingService[F] {

      def streamEvents(visitId: Visit.Id): Stream[F, TimeAccounting.Event] =
        session.stream(Statements.SelectEvents)(visitId, 1024)

      // Pipe that turns time accounting events into interval -> context pairs
      // so that they can be used to create TimeAccountingState
      val entriesPipe: Pipe[F, TimeAccounting.Event, (TimestampInterval, TimeAccounting.Context)] =
        _.groupAdjacentBy(_.context)
         .map { case (ctx, events) =>
            val head     = events.head.map(_.timestamp)
            val last     = events.last.map(_.timestamp)
            val interval =
              (head, last).mapN(TimestampInterval.between)
                          .getOrElse(sys.error("Stream.groupAdjacentBy produced and empty Chunk!"))

            interval -> ctx
         }

      // Pipe that fills in the gaps between steps with an interval -> context
      // pair (albeit with a None step context).
      def contiguousPipe(
        visitId: Visit.Id
      ): Pipe[F, (TimestampInterval, TimeAccounting.Context), (TimestampInterval, TimeAccounting.Context)] =
        _.zipWithNext
         .flatMap {
           case ((interval0, ctx0), None)                  =>
             Stream.emit(interval0 -> ctx0)
           case ((interval0, ctx0), Some(interval1, ctx1)) =>
             if (interval0.abuts(interval1)) Stream.emit(interval0 -> ctx0)
             else Stream(
               interval0 -> ctx0,
               TimestampInterval.between(interval0.end, interval1.start) -> TimeAccounting.Context(visitId, None)
             )
         }

      override def initialState(
        visitId: Visit.Id
      )(using xa: Transaction[F]): F[TimeAccountingState] =

        streamEvents(visitId)
          .through(entriesPipe)
          .through(contiguousPipe(visitId))
          .compile
          .toList
          .map(TimeAccountingState.fromSortedContiguousEntries)

    }

  object Statements {

    private object codec {
      val step_context: Decoder[TimeAccounting.StepContext] =
        (
          atom_id *:
          step_id *:
          obs_class.map(_.chargeClass)
        ).to[TimeAccounting.StepContext]

      val context: Decoder[TimeAccounting.Context] =
        (
          visit_id *:
          step_context.opt
        ).to[TimeAccounting.Context]

      val event: Decoder[TimeAccounting.Event] =
        (
          core_timestamp *:
          context
        ).to[TimeAccounting.Event]
    }

    val SelectEvents: Query[Visit.Id, TimeAccounting.Event] =
      sql"""
        SELECT
          e.c_received,
          e.c_visit_id,
          s.c_atom_id,
          e.c_step_id,
          s.c_observe_class
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
