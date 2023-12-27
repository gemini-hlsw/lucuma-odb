// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.TimeCharge
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*


trait TimeAccountingService[F[_]] {

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

  extension (night: ObservingNight) {

    /** Nautical twilight bounds cooresponding to an ObservingNight. */
    def nauticalTwilight: TimestampInterval = {
      // Twilight bounded night -- this is unsafe if the sun doesn't rise or
      // doesn't set on that observing night, but not an issue for GN or GS.
      val tbn = night.twilightBoundedUnsafe(TwilightType.Nautical)

      // Convert to a `TimeStampInterval`.  This is safe for dates between
      // [-4712 BC, 294,276 AD).
      TimestampInterval.between(
        Timestamp.unsafeFromInstantTruncated(tbn.start),
        Timestamp.unsafeFromInstantTruncated(tbn.end)
      )
    }

  }

  private def toDiscount(
    tas:     TimeAccountingState,
    comment: String
  ): Option[TimeCharge.Discount] =
    for {
      s <- tas.start
      e <- tas.end
      c  = tas.charge
    } yield TimeCharge.Discount(
      TimestampInterval.between(s, e),
      c.partnerTime,
      c.programTime,
      comment
    )

  def instantiate[F[_]: Concurrent](using Services[F]): TimeAccountingService[F] =
    new TimeAccountingService[F] {

      import Statements.*

      override def initialize(
        visitId: Visit.Id
      )(using Transaction[F]): F[Unit] =
        session.execute(SetTimeAccounting)((visitId, CategorizedTime.Zero, CategorizedTime.Zero)).void

      override def update(
        visitId: Visit.Id
      )(using Transaction[F]): F[Unit] =
        Update(visitId).run

      case class Update(visitId: Visit.Id)(using Transaction[F]) {
        lazy val run: F[Unit] =
          computeInvoice.flatMap(updateInvoice)

        private val initialState: F[TimeAccountingState] =
          for {
            o <- session.unique(SelectObservationId)(visitId)
            c <- session.unique(SelectObserveClass)(o)
            s <- session
                   .stream(SelectEvents)((c, visitId), 1024)
                   .through(TimeAccountingState.eventStreamPipe(c.chargeClass, visitId))
                   .compile
                   .onlyOrError
          } yield s

        private val daylightDiscounts: StateT[F, TimeAccountingState, List[TimeCharge.DiscountEntry]] =
          StateT { tas =>
            session.unique(SelectObservingNight)(visitId).map { optionNight =>
              optionNight.fold((tas, List.empty[TimeCharge.DiscountEntry])) { night =>
                val interval  = night.nauticalTwilight
                val preDusk   = toDiscount(tas.until(interval.start), TimeAccounting.comment.PreDusk)
                val postDawn  = toDiscount(tas.from(interval.end),    TimeAccounting.comment.PostDawn)
                val discounts = preDusk.toList ++ postDawn.toList
                (tas.between(interval), discounts.map(TimeCharge.DiscountEntry.Daylight(_, night.site)))
              }
            }
          }

        private val qaDiscounts: StateT[F, TimeAccountingState, List[TimeCharge.DiscountEntry]] =
          StateT { tas =>
            datasetService.selectDatasetsWithQaFailures(visitId).map { failures =>
              failures.toList.foldLeft((tas, List.empty[TimeCharge.DiscountEntry])) { case ((state, discounts), (atomId, datasets)) =>
                val (in, out) = state.partitionOnAtom(atomId)
                val discount  = toDiscount(in, TimeAccounting.comment.Qa).map { d =>
                  TimeCharge.DiscountEntry.Qa(d, datasets.toSet)
                }
                (out, discount.fold(discounts)(_ :: discounts))
              }
            }
          }

        private val computeInvoice: F[TimeCharge.Invoice] = {
          val invoice = for {
            ini <- StateT.get[F, TimeAccountingState]
            day <- daylightDiscounts
            qa  <- qaDiscounts
            // ... other discounts here ...
            fin <- StateT.get[F, TimeAccountingState]
          } yield TimeCharge.Invoice(ini.charge, day ++ qa, fin.charge)

          initialState.flatMap(invoice.runA)
        }

        private def updateInvoice(
          inv: TimeCharge.Invoice
        ): F[Unit] =
          for {
            _ <- updateDiscounts(inv.discounts)
            _ <- session.execute(UpdateTimeAccounting)((visitId, inv.executionTime, inv.finalCharge))
          } yield ()

        private def storeDiscount(d: TimeCharge.DiscountEntry): F[Unit] =
          for {
            id <- session.unique(StoreDiscountEntry)(visitId, d)
            _  <- d match {
              case TimeCharge.DiscountEntry.Qa(_, datasets) => datasets.toList.traverse_(session.execute(StoreQaDiscountDataset)(id, _))
              case _                                        => Applicative[F].unit
            }
          } yield ()

        private def updateDiscounts(
          discounts: List[TimeCharge.DiscountEntry]
        ): F[Unit] =
          for {
            _ <- session.execute(DeleteDiscountEntries)(visitId)
            _ <- discounts.traverse_(storeDiscount)
          } yield ()

      }

    }


  object Statements {

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

      val discount: Codec[TimeCharge.Discount] =
        (
          timestamp_interval *:
          time_span          *:
          time_span          *:
          text
        ).to[TimeCharge.Discount]

      val discount_entry: Encoder[TimeCharge.DiscountEntry] =
        (
          discount                  *:
          time_charge_discount_type *:
          site.opt
        ).contramap { entry => (
          entry.discount,
          entry.discriminator,
          entry match {
            case TimeCharge.DiscountEntry.Daylight(_, s) => s.some
            case _                                       => none
          }
        )}
    }

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

    val SelectObservingNight: Query[Visit.Id, Option[ObservingNight]] =
      sql"""
        SELECT
          c_instrument,
          c_created
        FROM
          t_visit
        WHERE
          c_visit_id = $visit_id
      """.query(instrument *: core_timestamp)
         .map {
            case (Instrument.GmosNorth, ts) => ObservingNight.fromSiteAndInstant(Site.GN, ts.toInstant).some
            case (Instrument.GmosSouth, ts) => ObservingNight.fromSiteAndInstant(Site.GS, ts.toInstant).some
            case _                          => none
         }

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

    val DeleteDiscountEntries: Command[Visit.Id] =
      sql"""
        DELETE FROM
          t_time_charge_discount
        WHERE
          c_visit_id = $visit_id
      """.command

    val StoreDiscountEntry: Query[(Visit.Id, TimeCharge.DiscountEntry), Long] =
      sql"""
        INSERT INTO
          t_time_charge_discount (
            c_visit_id,
            c_start,
            c_end,
            c_partner_discount,
            c_program_discount,
            c_comment,
            c_type,
            c_site
          )
        VALUES (
          $visit_id,
          ${codec.discount_entry}
        ) RETURNING
          c_id
      """.query(int8)

     val StoreQaDiscountDataset: Command[(Long, Dataset.Id)] =
       sql"""
         INSERT INTO
           t_time_charge_discount_dataset (
             c_discount_id,
             c_dataset_id
           )
         VALUES (
           $int8,
           $dataset_id
         )
       """.command
  }
}
