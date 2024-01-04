// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.eq.*
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
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.TimeCharge
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.codec.temporal.interval
import skunk.codec.text.text
import skunk.implicits.*

import java.time.Duration

import Services.Syntax.*


trait TimeAccountingService[F[_]] {

  /**
   * Updates the time accounting data.
   */
  def update(
    visitId: Visit.Id
  )(using Transaction[F]): F[Unit]

  /**
   * Adds a manual time charge correction.
   */
  def addCorrection(
    visitId:    Visit.Id,
    correction: TimeChargeCorrectionInput
  )(using Transaction[F]): F[Long]

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

  extension (time: CategorizedTime) {

    /** CategorizedTime after applying corrections. */
    def corrected(corrections: List[Correction]): CategorizedTime =
      corrections.foldLeft(time) { case (res, correction) =>
        correction.op match {
          case TimeChargeCorrection.Op.Add      => res.modify(correction.chargeClass, _ +| correction.amount)
          case TimeChargeCorrection.Op.Subtract => res.modify(correction.chargeClass, _ -| correction.amount)
        }
      }

  }

  private def toDiscount(tas: TimeAccountingState, comment: String): Option[TimeCharge.Discount] =
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

  case class Correction(
    chargeClass: ChargeClass,
    op:          TimeChargeCorrection.Op,
    amount:      TimeSpan
  )

  def instantiate[F[_]: Concurrent](using Services[F]): TimeAccountingService[F] =
    new TimeAccountingService[F] {

      import Statements.*

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
          def invoice(corrections: List[Correction]): StateT[F, TimeAccountingState, TimeCharge.Invoice] =
            for {
              ini <- StateT.get[F, TimeAccountingState]
              day <- daylightDiscounts
              qa  <- qaDiscounts
              // ... other discounts here ...
              fin <- StateT.get[F, TimeAccountingState]
            } yield TimeCharge.Invoice(ini.charge, day ++ qa, fin.charge.corrected(corrections))

          for {
            s <- initialState
            c <- session.execute(SelectCorrection)(visitId)
            i <- invoice(c).runA(s)
          } yield i
        }

        private def updateInvoice(inv: TimeCharge.Invoice): F[Unit] =
          for {
            _ <- updateDiscounts(inv.discounts)
            _ <- session.execute(UpdateTimeAccounting)((visitId, inv.executionTime, inv.finalCharge))
          } yield ()

        // Delete existing discounts (if any) and write the new ones.
        private def updateDiscounts(discounts: List[TimeCharge.DiscountEntry]): F[Unit] =
          for {
            _ <- session.execute(DeleteDiscountEntries)(visitId)
            _ <- discounts.traverse_(storeDiscount)
          } yield ()

        // Stores the discount entry.
        private def storeDiscount(d: TimeCharge.DiscountEntry): F[Unit] =
          for {
            id <- session.unique(StoreDiscountEntry)(visitId, d)
            _  <- storeDiscountDetail(id)(d)
          } yield ()

        // Stores any information specific to particular discount entry types.
        private def storeDiscountDetail(id: Long): TimeCharge.DiscountEntry => F[Unit] = {
          case TimeCharge.DiscountEntry.Qa(_, datasets) =>
            datasets.toList.traverse_(session.execute(StoreQaDiscountDataset)(id, _))
          case _                                        =>
            Applicative[F].unit
        }
      }

      def addCorrection(
        visitId: Visit.Id,
        input:   TimeChargeCorrectionInput
      )(using Transaction[F]): F[Long] =
        for {
          id <- session.unique(StoreCorrection)(visitId, input, user.id)
          _  <- session.execute(PerformCorrection)(visitId, input).void
        } yield id

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

      val correction: Codec[Correction] =
        (charge_class              *:
         time_charge_correction_op *:
         time_span
        ).to[Correction]
    }

    val UpdateTimeAccounting: Command[(
      Visit.Id,
      CategorizedTime,
      CategorizedTime
    )] =
      sql"""
        UPDATE t_visit
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

    val StoreCorrection: Query[(Visit.Id, TimeChargeCorrectionInput, User.Id), Long] =
      sql"""
        INSERT INTO
          t_time_charge_correction (
            c_visit_id,
            c_charge_class,
            c_op,
            c_amount,
            c_comment,
            c_user_id
          )
        VALUES(
          $visit_id,
          $charge_class,
          $time_charge_correction_op,
          $time_span,
          ${text_nonempty.opt},
          $user_id
        ) RETURNING
          c_id
      """.query(int8)
         .contramap { (vid, tcc, uid) => (
           vid,
           tcc.chargeClass,
           tcc.op,
           tcc.amount,
           tcc.comment,
           uid
         )}

    val SelectCorrection: Query[Visit.Id, Correction] =
      sql"""
        SELECT
          c_charge_class,
          c_op,
          c_amount
        FROM
          t_time_charge_correction
        WHERE
          c_visit_id = $visit_id
        ORDER BY c_created
      """.query(codec.correction)

    val PerformCorrection: Command[(Visit.Id, TimeChargeCorrectionInput)] = {
      def amount(input: TimeChargeCorrectionInput): Duration =
        input.op match {
          case TimeChargeCorrection.Op.Add      => input.amount.toDuration
          case TimeChargeCorrection.Op.Subtract => input.amount.toDuration.negated
        }

      def toDuration(chargeClass: ChargeClass, input: TimeChargeCorrectionInput): Duration =
        if (input.chargeClass =!= chargeClass) Duration.ZERO
        else amount(input)

      sql"""
        UPDATE t_visit
           SET c_final_partner_time = update_time_span(c_final_partner_time, $interval),
               c_final_program_time = update_time_span(c_final_program_time, $interval)
         WHERE c_visit_id = $visit_id
      """.command.contramap { case (vid, input) => (
        toDuration(ChargeClass.Partner, input),
        toDuration(ChargeClass.Program, input),
        vid
      )}
    }
  }
}
