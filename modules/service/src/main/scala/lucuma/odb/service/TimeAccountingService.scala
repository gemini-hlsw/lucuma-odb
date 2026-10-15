// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.data.StateT
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.BandedSetupCharge
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.SetupCharge
import lucuma.core.model.sequence.SetupChargeSummary
import lucuma.core.model.sequence.SetupTimeBreakdown
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.TimeCharge
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.numeric.int4
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
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  /**
   * Updates the time accounting data for every visit of the given observation.
   * A no-op for observations that have no visits (i.e., that haven't begun
   * executing).
   */
  def updateAll(
    observationId: Observation.Id
  )(using Transaction[F], Services.ServiceAccess): F[Unit]

  /**
   * Adds a manual time charge correction.
   */
  def addCorrection(
    visitId:    Visit.Id,
    correction: TimeChargeCorrectionInput
  )(using Transaction[F], Services.StaffAccess): F[Long]

  /**
   * Sums the final time accounting result for all visits of an observation.
   */
  def selectObservation(
    observationId: Observation.Id
  )(using Transaction[F], SuperUserAccess): F[Option[CategorizedTime]]

  /**
   * Sums the final time accounting result for each of the given observations. Observations with
   * no visits are absent from the resulting map.
   */
  def selectObservations(
    observationIds: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, CategorizedTime]]

  /**
   * Sums the final time accounting result for all observations of a program.
   */
  def selectProgram(
    programId: Program.Id
  )(using Transaction[F], SuperUserAccess): F[List[BandedTime]]

  /**
   * Selects the setups recorded for a visit, in order.
   */
  def selectVisitSetups(
    visitId: Visit.Id
  )(using Transaction[F], SuperUserAccess): F[List[TimeAccounting.Setup]]

  /**
   * Sums the setups recorded for each of the given observations.  Observations
   * with no recorded setups are absent from the resulting map.
   */
  def selectObservationsSetupCharge(
    observationIds: List[Observation.Id]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, SetupCharge]]

  /**
   * Sums the setups recorded for all observations of a program, by band.
   */
  def selectProgramSetupCharge(
    programId: Program.Id
  )(using Transaction[F], SuperUserAccess): F[List[BandedSetupCharge]]
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
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        Update(visitId).run

      override def updateAll(
        observationId: Observation.Id
      )(using Transaction[F], Services.ServiceAccess): F[Unit] =
        // Recompute only the visits that were actually invalidated (received an
        // execution event or a dataset QA-state change) since their last update.
        // Visits with no relevant change -- including closed, already-corrected
        // visits of the same observation -- are left untouched, so an unrelated
        // obscalc invalidation (or an event in a different visit) never reprices
        // them.  We record the invalidation timestamp we observed (not `now()`)
        // so that an invalidation arriving during the recompute leaves the visit
        // dirty for the next pass.
        session.execute(SelectDirtyVisits)(observationId).flatMap:
          _.traverse_ { (visitId, invalidation) =>
            update(visitId) *>
            session.execute(MarkVisitTimeAccountingUpdated)(invalidation, visitId).void
          }

      case class Update(visitId: Visit.Id)(using Transaction[F]) {
        lazy val run: F[Unit] =
          compute.flatMap { (inv, setups) =>
            updateInvoice(inv) *> updateSetups(setups)
          }

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
          StateT: tas =>
            session.unique(SelectObservingNight)(visitId).map: night =>
              val interval  = night.nauticalTwilight
              val preDusk   = toDiscount(tas.until(interval.start), TimeAccounting.comment.PreDusk)
              val postDawn  = toDiscount(tas.from(interval.end),    TimeAccounting.comment.PostDawn)
              val discounts = preDusk.toList ++ postDawn.toList
              (tas.between(interval), discounts.map(TimeCharge.DiscountEntry.Daylight(_, night.site)))

        private val noDataDiscount: StateT[F, TimeAccountingState, List[TimeCharge.DiscountEntry]] =
          StateT { tas =>
            datasetService.hasDatasets(visitId).map {
              case false => (TimeAccountingState.Empty,
                             toDiscount(tas, TimeAccounting.comment.NoData)
                               .map(TimeCharge.DiscountEntry.NoData.apply)
                               .toList
                            )
              case _     => (tas, Nil)
            }
          }

        private val overlapDiscounts: StateT[F, TimeAccountingState, List[TimeCharge.DiscountEntry]] =
          StateT: tas =>
            session
              .option(SelectChargeableOverlap)(visitId)
              .map:
                case Some((overlapTime, oid)) =>
                  val discount = toDiscount(tas.from(overlapTime), TimeAccounting.comment.Overlap).map: d =>
                     TimeCharge.DiscountEntry.Overlap(d, oid)
                  (tas.until(overlapTime), discount.toList)
                case None                     =>
                  (tas, Nil)

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

        // Computes the invoice and the setups, which are found in the complete
        // visit state but charged according to what remains after discounts.
        private val compute: F[(TimeCharge.Invoice, List[TimeAccounting.Setup])] = {
          def invoice(corrections: List[Correction]): StateT[F, TimeAccountingState, (TimeCharge.Invoice, List[TimeAccounting.Setup])] =
            for {
              ini <- StateT.get[F, TimeAccountingState]
              nod <- noDataDiscount
              day <- daylightDiscounts
              ovr <- overlapDiscounts
              qa  <- qaDiscounts
              // ... other discounts here ...
              fin <- StateT.get[F, TimeAccountingState]
            } yield (
              TimeCharge.Invoice(ini.charge, nod ++ day ++ ovr ++ qa, fin.charge.corrected(corrections)),
              ini.setups(fin)
            )

          for {
            s <- initialState
            c <- session.execute(SelectCorrection)(visitId)
            i <- invoice(c).runA(s)
          } yield i
        }

        // Replaces the visit's setups.
        private def updateSetups(setups: List[TimeAccounting.Setup]): F[Unit] =
          for {
            _ <- session.execute(DeleteSetups)(visitId)
            _ <- setups.zipWithIndex.traverse_ { (setup, idx) => session.execute(InsertSetup)(visitId, idx, setup) }
          } yield ()

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
          case TimeCharge.DiscountEntry.Overlap(_, oid) =>
            session.execute(StoreOverlapDiscountObservation)(id, oid).void
          case TimeCharge.DiscountEntry.Qa(_, datasets) =>
            datasets.toList.traverse_(session.execute(StoreQaDiscountDataset)(id, _))
          case _                                        =>
            Applicative[F].unit
        }
      }

      def addCorrection(
        visitId: Visit.Id,
        input:   TimeChargeCorrectionInput
      )(using Transaction[F], Services.StaffAccess): F[Long] =
        for {
          id <- session.unique(StoreCorrection)(visitId, input, user.id)
          _  <- session.execute(PerformCorrection)(visitId, input).void
        } yield id

      def selectObservation(
        observationId: Observation.Id
      )(using Transaction[F], SuperUserAccess): F[Option[CategorizedTime]] =
        session
          .option(SelectObservation)(observationId)

      def selectObservations(
        observationIds: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, CategorizedTime]] =
        NonEmptyList.fromList(observationIds.distinct) match
          case None      => Map.empty.pure
          case Some(nel) =>
            session.execute(Statements.selectObservations(nel))(nel).map(_.toMap)

      def selectProgram(
        programId: Program.Id
      )(using Transaction[F], SuperUserAccess): F[List[BandedTime]] =
        session
          .execute(SelectProgram)(programId)

      def selectVisitSetups(
        visitId: Visit.Id
      )(using Transaction[F], SuperUserAccess): F[List[TimeAccounting.Setup]] =
        session.execute(SelectVisitSetups)(visitId)

      def selectObservationsSetupCharge(
        observationIds: List[Observation.Id]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, SetupCharge]] =
        NonEmptyList.fromList(observationIds.distinct) match
          case None      => Map.empty.pure
          case Some(nel) =>
            session
              .execute(Statements.selectObservationsSetupCharge(nel))(nel)
              .map(_.groupMapReduce(_._1)(_._2)(_ |+| _))

      def selectProgramSetupCharge(
        programId: Program.Id
      )(using Transaction[F], SuperUserAccess): F[List[BandedSetupCharge]] =
        session
          .execute(SelectProgramSetupCharge)(programId)
          .map { rows =>
            // Rows are sorted by band, so grouping adjacent rows preserves the order.
            rows.foldRight(List.empty[BandedSetupCharge]) {
              case ((band, charge), BandedSetupCharge(b, c) :: tail) if b === band =>
                BandedSetupCharge(band, charge |+| c) :: tail
              case ((band, charge), acc) =>
                BandedSetupCharge(band, charge) :: acc
            }
          }

    }

  object Statements {

    private object codec {

      val atom_context: Decoder[TimeAccounting.AtomContext] =
        (atom_id *: step_id.opt *: sequence_type).to[TimeAccounting.AtomContext]

      // Whether the event starts or continues a slew is determined later, when
      // the event stream is processed.
      val context: Decoder[TimeAccounting.Context] =
        (
          visit_id                     *:
          obs_class.map(_.chargeClass) *:
          atom_context.opt
        ).map { (v, c, a) => TimeAccounting.Context(v, c, a, slewing = false) }

      val event: Decoder[TimeAccounting.Event] =
        (core_timestamp *: context *: slew_stage.opt).to[TimeAccounting.Event]

      val setup_time_breakdown: Codec[SetupTimeBreakdown] =
        (categorized_time *: categorized_time *: categorized_time).to[SetupTimeBreakdown]

      val setup: Codec[TimeAccounting.Setup] =
        (setup_kind *: timestamp_interval *: setup_time_breakdown).to[TimeAccounting.Setup]

      // A count and summed time for setups of a single kind.
      val setup_charge: Decoder[SetupCharge] =
        (setup_kind *: int8 *: setup_time_breakdown).map { (kind, count, time) =>
          SetupCharge.of(
            kind,
            SetupChargeSummary(NonNegInt.unsafeFrom(count.min(Int.MaxValue.toLong).toInt), time)
          )
        }

      val discount: Codec[TimeCharge.Discount] =
        (
          timestamp_interval *:
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

      val banded_time: Codec[BandedTime] =
        (science_band.opt *: categorized_time).to[BandedTime]
    }

    val UpdateTimeAccounting: Command[(
      Visit.Id,
      CategorizedTime,
      CategorizedTime
    )] =
      sql"""
        UPDATE t_visit
           SET c_raw_non_charged_time   = $time_span,
               c_raw_program_time       = $time_span,
               c_final_non_charged_time = $time_span,
               c_final_program_time     = $time_span
         WHERE c_visit_id = $visit_id
      """.command
         .contramap { case (v, r, f) => (
           r.nonCharged,
           r.programTime,
           f.nonCharged,
           f.programTime,
           v
         )}

    // The observation's visits that are "dirty" (invalidated since their last
    // update), each with the invalidation timestamp to record once recomputed.
    val SelectDirtyVisits: Query[Observation.Id, (Visit.Id, Timestamp)] =
      sql"""
        SELECT
          c_visit_id,
          c_ta_invalidation
        FROM
          t_visit
        WHERE
          c_observation_id  = $observation_id AND
          c_ta_invalidation > c_ta_update
      """.query(visit_id *: core_timestamp)

    val MarkVisitTimeAccountingUpdated: Command[(Timestamp, Visit.Id)] =
      sql"""
        UPDATE t_visit
           SET c_ta_update = $core_timestamp
         WHERE c_visit_id = $visit_id
      """.command

    val SelectObservationId: Query[Visit.Id, Observation.Id] =
      sql"""
        SELECT
          c_observation_id
        FROM
          t_visit
        WHERE
          c_visit_id = $visit_id
      """.query(observation_id)

    val SelectObservingNight: Query[Visit.Id, ObservingNight] =
      sql"""
        SELECT
          c_site,
          c_effective_time
        FROM
          t_visit
        WHERE
          c_visit_id = $visit_id
      """.query(site *: core_timestamp)
         .map: (site, ts) =>
            ObservingNight.fromSiteAndInstant(site, ts.toInstant)

    val SelectObserveClass: Query[Observation.Id, ObserveClass] =
      sql"""
        SELECT
          COALESCE(MIN(s.c_observe_class), 'science'::e_obs_class)
        FROM
          t_step s
        INNER JOIN t_atom a USING (c_atom_id)
        WHERE
          a.c_observation_id = $observation_id
      """.query(obs_class)

    val SelectEvents: Query[(ObserveClass, Visit.Id), TimeAccounting.Event] =
      sql"""
        SELECT
          e.c_effective_time,
          e.c_visit_id,
          COALESCE(s.c_observe_class, $obs_class),
          e.c_atom_id,
          e.c_step_id,
          a.c_sequence_type,
          e.c_slew_stage
        FROM
          t_execution_event e
        LEFT JOIN t_step s USING (c_step_id)
        LEFT JOIN t_atom a ON a.c_atom_id = e.c_atom_id
        WHERE
          e.c_visit_id = $visit_id
        ORDER BY
          e.c_effective_time, e.c_execution_event_id
      """.query(codec.event)

    val SelectChargeableOverlap: Query[Visit.Id, (Timestamp, Observation.Id)] =
      sql"""
        WITH v AS (
          SELECT
            c_visit_id,
            c_site,
            c_start,
            c_end
          FROM t_visit
          WHERE c_visit_id = $visit_id
        )
        SELECT
          v2.c_start,
          v2.c_observation_id
        FROM
          t_visit v2, v
        WHERE
          v2.c_visit_id != v.c_visit_id AND
          v2.c_site = v.c_site          AND
          v2.c_chargeable = true        AND
          v2.c_start < v.c_end          AND
          (
            -- The usual case if v2 overlaps
            v2.c_start > v.c_start                                    OR

            -- The edge case where the start times are the same on two visits.
            -- Here we just pick the one with the newer id to be charged.
            (v2.c_start = v.c_start AND v2.c_visit_id > v.c_visit_id)

            -- If v2.c_start < v.c_start then these don't overlap
          )
        ORDER BY v2.c_start LIMIT 1; -- pick the first one that overlaps
      """.query(core_timestamp *: observation_id)

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
            c_amount,
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

     val StoreOverlapDiscountObservation: Command[(Long, Observation.Id)] =
       sql"""
         INSERT INTO
           t_time_charge_discount_overlap (
             c_discount_id,
             c_observation_id
           )
         VALUES (
           $int8,
           $observation_id
         )
       """.command

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
           SET c_final_program_time = update_time_span(c_final_program_time, $interval)
         WHERE c_visit_id = $visit_id
      """.command.contramap { case (vid, input) => (
        toDuration(ChargeClass.Program, input),
        vid
      )}
    }

    val SelectObservation: Query[Observation.Id, CategorizedTime] =
      sql"""
        SELECT
          COALESCE(SUM(c_final_non_charged_time), '0'::interval),
          COALESCE(SUM(c_final_program_time), '0'::interval)
        FROM t_visit
        WHERE c_observation_id = $observation_id
      """.query(categorized_time)

    def selectObservations(
      oids: NonEmptyList[Observation.Id]
    ): Query[oids.type, (Observation.Id, CategorizedTime)] =
      sql"""
        SELECT
          c_observation_id,
          COALESCE(SUM(c_final_non_charged_time), '0'::interval),
          COALESCE(SUM(c_final_program_time), '0'::interval)
        FROM t_visit
        WHERE c_observation_id IN (${observation_id.nel(oids)})
        GROUP BY c_observation_id
      """.query(observation_id *: categorized_time)

    // Sums of the setup time columns, in SetupTimeBreakdown order.
    private val SetupTimeSums: String =
      """
        COALESCE(SUM(s.c_slew_non_charged_time),  '0'::interval),
        COALESCE(SUM(s.c_slew_program_time),      '0'::interval),
        COALESCE(SUM(s.c_acq_non_charged_time),   '0'::interval),
        COALESCE(SUM(s.c_acq_program_time),       '0'::interval),
        COALESCE(SUM(s.c_other_non_charged_time), '0'::interval),
        COALESCE(SUM(s.c_other_program_time),     '0'::interval)
      """

    val DeleteSetups: Command[Visit.Id] =
      sql"""
        DELETE FROM t_visit_setup WHERE c_visit_id = $visit_id
      """.command

    val InsertSetup: Command[(Visit.Id, Int, TimeAccounting.Setup)] =
      sql"""
        INSERT INTO t_visit_setup (
          c_visit_id,
          c_index,
          c_kind,
          c_start,
          c_end,
          c_slew_non_charged_time,
          c_slew_program_time,
          c_acq_non_charged_time,
          c_acq_program_time,
          c_other_non_charged_time,
          c_other_program_time
        ) VALUES (
          $visit_id,
          $int4,
          ${codec.setup}
        )
      """.command

    val SelectVisitSetups: Query[Visit.Id, TimeAccounting.Setup] =
      sql"""
        SELECT
          c_kind,
          c_start,
          c_end,
          c_slew_non_charged_time,
          c_slew_program_time,
          c_acq_non_charged_time,
          c_acq_program_time,
          c_other_non_charged_time,
          c_other_program_time
        FROM t_visit_setup
        WHERE c_visit_id = $visit_id
        ORDER BY c_index
      """.query(codec.setup)

    def selectObservationsSetupCharge(
      oids: NonEmptyList[Observation.Id]
    ): Query[oids.type, (Observation.Id, SetupCharge)] =
      sql"""
        SELECT
          v.c_observation_id,
          s.c_kind,
          COUNT(*),
          #$SetupTimeSums
        FROM t_visit_setup s
        INNER JOIN t_visit v ON v.c_visit_id = s.c_visit_id
        WHERE v.c_observation_id IN (${observation_id.nel(oids)})
        GROUP BY v.c_observation_id, s.c_kind
      """.query(observation_id *: codec.setup_charge)

    val SelectProgramSetupCharge: Query[Program.Id, (Option[ScienceBand], SetupCharge)] =
      sql"""
        SELECT
          o.c_science_band,
          s.c_kind,
          COUNT(*),
          #$SetupTimeSums
        FROM t_visit_setup s
        INNER JOIN t_visit v ON v.c_visit_id = s.c_visit_id
        INNER JOIN t_observation o ON o.c_observation_id = v.c_observation_id
        WHERE o.c_program_id = $program_id
        GROUP BY o.c_science_band, s.c_kind
        ORDER BY o.c_science_band ASC NULLS FIRST
      """.query(science_band.opt *: codec.setup_charge)

    val SelectProgram: Query[Program.Id, BandedTime] =
      sql"""
        SELECT
          o.c_science_band,
          COALESCE(SUM(v.c_final_non_charged_time), '0'::interval),
          COALESCE(SUM(v.c_final_program_time), '0'::interval)
        FROM t_visit v
        INNER JOIN t_observation o ON v.c_observation_id = o.c_observation_id
        WHERE o.c_program_id = $program_id
        GROUP BY o.c_science_band
        ORDER BY o.c_science_band ASC NULLS FIRST
      """.query(codec.banded_time)
  }
}
