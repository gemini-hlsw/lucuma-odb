// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Pure
import fs2.Stream
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.model.Observation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import lucuma.odb.data.Obscalc
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.numeric.int4
import skunk.implicits.*

sealed trait ObscalcService[F[_]]:

  /**
   * Selects Obscalc data for a single observation, if the observation exists
   * and there is an obscalc entry for it.
   */
  def selectOne(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[Obscalc.Entry]]

  /**
   * Selects Obscalc data for the given observations, where the observation
   * exists and there is an obscalc entry for it.
   */
  def selectMany(
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, Obscalc.Entry]]

  /**
   * Selects Obscalc data for all observations in a program for which a result
   * exists.
   */
  def selectProgram(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Observation.Id, Obscalc.Entry]]

  /**
   * Select calculated categorized time for a single observation.
   */
  def selectOneCategorizedTime(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTime]]]

  /**
   * Select calculated categorized time for all observations in a program.
   */
  def selectProgramCategorizedTime(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Observation.Id, CalculatedValue[CategorizedTime]]]

  /**
   * Selects the execution digest corresponding to the given observation id.
   */
  def selectExecutionDigest(
    observationId: Observation.Id
  )(using Transaction[F]): F[Option[CalculatedValue[Result[ExecutionDigest]]]]

  /**
   * Selects the execution digests corresponding to the given observation ids.
   */
  def selectManyExecutionDigest(
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, CalculatedValue[Result[ExecutionDigest]]]]

  /**
   * Marks all 'calculating' observations as 'pending' (or 'retry' if
   * appropriate). This is intended to be used by the worker service on startup
   * to cleanup state and restart.
   */
  def reset(using ServiceAccess, Transaction[F]): F[Unit]

  /**
   * Loads up to `max` `pending` (or `retry`) calculations.  Loading changes
   * the state of the entries to `calculating` before they are returned.
   */
  def load(max: Int)(using ServiceAccess, Transaction[F]): F[List[Obscalc.PendingCalc]]

  /**
   * Loads the PendingCalc entry for the given observation, if it exists and is
   * in fact `pending` (or `retry`).  The state is update to `calculating`
   * before the entry is returned.
   */
  def loadObs(
    observationId: Observation.Id
  )(using ServiceAccess, Transaction[F]): F[Option[Obscalc.PendingCalc]]

  /**
   * Calculates the result for the associated observation and updates the
   * entry accordingly.
   */
  def calculateAndUpdate(
    pending: Obscalc.PendingCalc
  )(using ServiceAccess, NoTransaction[F]): F[Option[Obscalc.Meta]]

  /**
   * Calculates and returns the result for the associated observation without
   * updating the database.  This is intended primarily to facilitate testing.
   * Instead, `calculateAndUpdate` is provided for the Obscalc worker service
   * that is keeping everything up-to-date.
   */
  def calculateOnly(
    pending: Obscalc.PendingCalc
  )(using ServiceAccess, NoTransaction[F]): F[Obscalc.Result]

object ObscalcService:

  /**
   * Default workflow instance to use when workflow cannot be computed.
   */
  val UndefinedWorkflow = ObservationWorkflow(
    ObservationWorkflowState.Undefined,
    List(ObservationWorkflowState.Inactive),
    Nil
  )

  def instantiate[F[_]: Concurrent: Logger: Services]: ObscalcService[F] =

    new ObscalcService[F]:
      override def selectOne(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[Obscalc.Entry]] =
        session.option(Statements.SelectOne)(observationId)

      override def selectMany(
        observationIds: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, Obscalc.Entry]] =
        NonEmptyList.fromList(observationIds) match
          case None      => Map.empty.pure
          case Some(nel) =>
            val enc = observation_id.nel(nel)
            session
              .stream(Statements.selectMany(enc))(nel, 1024)
              .compile
              .toList
              .map(_.fproductLeft(_.meta.observationId).toMap)

      override def selectProgram(
        programId: Program.Id
      )(using Transaction[F]): F[Map[Observation.Id, Obscalc.Entry]] =
        session
          .execute(Statements.SelectProgram)(programId)
          .map(_.fproductLeft(_.meta.observationId).toMap)

      override def selectOneCategorizedTime(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTime]]] =
        session.option(Statements.SelectOneCategorizedTime)(observationId)

      override def selectProgramCategorizedTime(
        programId: Program.Id
      )(using Transaction[F]): F[Map[Observation.Id, CalculatedValue[CategorizedTime]]] =
        session
          .execute(Statements.SelectProgramCategorizedTime)(programId)
          .map(_.toMap)

      override def selectExecutionDigest(
        observationId: Observation.Id
      )(using Transaction[F]): F[Option[CalculatedValue[Result[ExecutionDigest]]]] =
        selectManyExecutionDigest(List(observationId))
          .map(_.get(observationId))

      override def selectManyExecutionDigest(
        observationIds: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, CalculatedValue[Result[ExecutionDigest]]]] =
        NonEmptyList
          .fromList(observationIds)
          .fold(Map.empty[Observation.Id, CalculatedValue[Result[ExecutionDigest]]].pure[F]): nel =>
            val af = Statements.selectManyExecutionDigest(nel)
            session
              .prepareR(af.fragment.query(
                observation_id                  *:
                calculation_state               *:
                odb_error.opt                   *:
                execution_digest.opt
              ))
              .use(_.stream(af.argument, chunkSize = 64).compile.to(List))
              .map: lst =>
                lst
                  .map:
                    case (oid, state, None, None)                =>
                      oid -> CalculatedValue(state, OdbError.SequenceUnavailable(oid, s"The background calculation has not (yet) produced a value for observation $oid".some).asFailure)
                    case (oid, state, Some(error), None)         =>
                      oid -> CalculatedValue(state, error.asFailure)
                    case (oid, state, None, Some(digest))        =>
                      oid -> CalculatedValue(state, digest.success)
                    case (oid, state, Some(error), Some(digest)) =>
                      oid -> CalculatedValue(state, error.asWarning(digest))
                  .toMap

      override def reset(using ServiceAccess, Transaction[F]): F[Unit] =
        session.execute(Statements.ResetCalculating).void

      override def load(
        max: Int
      )(using ServiceAccess, Transaction[F]): F[List[Obscalc.PendingCalc]] =
        session.execute(Statements.LoadPendingCalc)(max)

      override def loadObs(
        observationId: Observation.Id
      )(using ServiceAccess, Transaction[F]): F[Option[Obscalc.PendingCalc]] =
        session.option(Statements.LoadPendingCalcFor)(observationId)

      override def calculateOnly(
        pending: Obscalc.PendingCalc
      )(using ServiceAccess, NoTransaction[F]): F[Obscalc.Result] =
        calculateWithAtomDigests(pending).map(_._1)

      private def calculateWithAtomDigests(
        pending: Obscalc.PendingCalc
      )(using ServiceAccess): F[(Obscalc.Result, Stream[Pure, AtomDigest])] =

        def sequenceUnavailable(msg: String): OdbError =
          OdbError.SequenceUnavailable(pending.observationId, s"Could not generate a sequence for ${pending.observationId}: $msg".some)

        val params: EitherT[F, OdbError, GeneratorParams] =
          EitherT:
            services.transactionally:
              generatorParamsService
                .selectOne(pending.programId, pending.observationId)
                .map(_.leftMap(e => sequenceUnavailable(e.format)))

        def workflow(
          itc: Option[ItcService.AsterismResults],
          dig: Option[ExecutionDigest]
        ): F[ObservationWorkflow] =
          Logger[F].info(s"${pending.observationId}: calculating workflow") *>
          services
            .transactionally:
              observationWorkflowService.getCalculatedWorkflow(pending.observationId, itc, dig.map(_.science.executionState))
            .flatMap: r =>
              r.toOption.fold(Logger[F].warn(s"${pending.observationId}: failure calculating workflow: $r").as(UndefinedWorkflow))(_.pure[F])
            .flatTap: r =>
              Logger[F].info(s"${pending.observationId}: finished calculating workflow: $r")

        val gen = generator

        def digest(itcResult: Either[OdbError, ItcService.AsterismResults]): F[Either[OdbError, (ExecutionDigest, Stream[Pure, AtomDigest])]] =
          Logger[F].info(s"${pending.observationId}: calculating digest") *>
          ((for
            p <- params
            d <- EitherT(gen.calculateDigest(pending.programId, pending.observationId, itcResult, p))
            a <- EitherT(gen.calculateScienceAtomDigests(pending.programId, pending.observationId, itcResult, p))
          yield (d, a)).value).flatTap: da =>
            Logger[F].info(s"${pending.observationId}: finished calculting digest: $da")

        val result = for
          r <- itcService.lookup(pending.programId, pending.observationId)
          _ <- Logger[F].info(s"${pending.observationId}: itc lookup: $r")
          d <- digest(r)
          w <- workflow(r.toOption, d.toOption.map(_._1))
        yield d.fold(
          err => (Obscalc.Result.Error(err, w), Stream.empty),
          dig => (
            r.fold(
              _ => Obscalc.Result.WithoutTarget(dig._1, w),
              i => Obscalc.Result.WithTarget(Obscalc.ItcResult(i.acquisitionResult.focus, i.scienceResult.focus), dig._1, w)
            ),
            dig._2
          )
        )

        Logger[F].info(s"${pending.observationId}: *** start calculating") *>
        result.flatTap: r =>
          Logger[F].info(s"${pending.observationId}: *** end calculating: $r")

      @annotation.nowarn("msg=unused implicit parameter")
      private def storeResult(
        pending:  Obscalc.PendingCalc,
        result:   Obscalc.Result,
        expected: CalculationState
      )(using ServiceAccess, Transaction[F]): F[Option[Obscalc.Meta]] =
        for
          lu <- session.option(Statements.SelectLastInvalidationForUpdate)(pending.observationId)
          ns  = lu.map(lastUpdate => if lastUpdate === pending.lastInvalidation then expected else CalculationState.Pending)
          af  = ns.map(newState => Statements.storeResult(pending, result, newState))
          m  <- af.traverse(f => session.unique(f.fragment.query(Statements.obscalc_meta))(f.argument))
        yield m

      override def calculateAndUpdate(
        pending: Obscalc.PendingCalc
      )(using ServiceAccess, NoTransaction[F]): F[Option[Obscalc.Meta]] =
        calculateWithAtomDigests(pending)
          .flatMap: (result, atomDigests) =>
            services.transactionally:
              sequenceService.insertAtomDigests(pending.observationId, atomDigests) *>
              (result.odbError match
                case Some(OdbError.RemoteServiceCallError(_)) => storeResult(pending, result, CalculationState.Retry)
                case _                                        => storeResult(pending, result, CalculationState.Ready))
          .handleErrorWith: e =>
            val result = Obscalc.Result.Error(OdbError.UpdateFailed(Option(e.getMessage)), UndefinedWorkflow)
            services.transactionally:
              storeResult(pending, result, CalculationState.Retry)

  object Statements:
    val pending_obscalc: Codec[Obscalc.PendingCalc] =
      (program_id *: observation_id *: core_timestamp).to[Obscalc.PendingCalc]

    val obscalc_meta: Codec[Obscalc.Meta] = (
      program_id         *: // c_program_id
      observation_id     *: // c_observation_id
      calculation_state  *: // c_obscalc_state
      core_timestamp     *: // c_last_invalidation
      core_timestamp     *: // c_last_update
      core_timestamp.opt *: // c_retry_at
      int4_nonneg           // c_failure_count
    ).to[Obscalc.Meta]

    val integration_time: Codec[IntegrationTime] =
      (time_span *: int4_pos).to[IntegrationTime]

    val signal_to_noise_at: Codec[SignalToNoiseAt] =
      (wavelength_pm *: signal_to_noise *: signal_to_noise)
        .imap((w, s, t) => SignalToNoiseAt(w, SingleSN(s), TotalSN(t)))(
          sna => (sna.wavelength, sna.single.value, sna.total.value)
        )

    val target_result: Codec[ItcService.TargetResult] =
      (target_id *: integration_time *: signal_to_noise_at.opt).to[ItcService.TargetResult]

    val itc_result: Codec[Obscalc.ItcResult] =
      (target_result *: target_result).to[Obscalc.ItcResult]

    val observation_workflow: Codec[ObservationWorkflow] =
      (observation_workflow_state *: _observation_workflow_state *: _observation_validation).to[ObservationWorkflow]

    val obscalc_result_opt: Codec[Option[Obscalc.Result]] =
      (odb_error.opt *: itc_result.opt *: execution_digest.opt *: observation_workflow).eimap {
        case (None, None, None, _)        => none.asRight
        case (Some(e), None, None, wf)    => Obscalc.Result.Error(e, wf).some.asRight
        case (None, None, Some(d), wf)    => Obscalc.Result.WithoutTarget(d, wf).some.asRight
        case (None, Some(i), Some(d), wf) => Obscalc.Result.WithTarget(i, d, wf).some.asRight
        case (e, i, d, wf)                => s"Could not decode obscalc result: $e, $i, $d, $wf".asLeft
      }(r => (r.flatMap(_.odbError), r.flatMap(_.itcResult), r.flatMap(_.digest), r.map(_.workflow).getOrElse(UndefinedWorkflow)))

    val obscalc_entry: Codec[Obscalc.Entry] =
      (obscalc_meta *: obscalc_result_opt).to[Obscalc.Entry]

    private def prefixedColumns(prefix: Option[String], colNames: String*): String =
      colNames.toList.map: col =>
        prefix.fold(col)(p => s"$p.$col")
      .mkString("", ",\n", "\n")

    private def obscalcMetaDataColumns(prefix: Option[String]): String =
      prefixedColumns(
        prefix,
        "c_program_id",
        "c_observation_id",
        "c_obscalc_state",
        "c_last_invalidation",
        "c_last_update",
        "c_retry_at",
        "c_failure_count"
      )

    private def obscalcResultColumns(prefix: Option[String]): String =
      prefixedColumns(
        prefix,
        "c_odb_error",

        "c_img_target_id",
        "c_img_exposure_time",
        "c_img_exposure_count",
        "c_img_wavelength",
        "c_img_single_sn",
        "c_img_total_sn",

        "c_spec_target_id",
        "c_spec_exposure_time",
        "c_spec_exposure_count",
        "c_spec_wavelength",
        "c_spec_single_sn",
        "c_spec_total_sn",

        "c_full_setup_time",
        "c_reacq_setup_time",

        "c_acq_obs_class",
        "c_acq_non_charged_time",
        "c_acq_program_time",
        "c_acq_offsets",
        "c_acq_offset_guide_states",
        "c_acq_atom_count",
        "c_acq_execution_state",

        "c_sci_obs_class",
        "c_sci_non_charged_time",
        "c_sci_program_time",
        "c_sci_offsets",
        "c_sci_offset_guide_states",
        "c_sci_atom_count",
        "c_sci_execution_state",

        "c_workflow_state",
        "c_workflow_transitions",
        "c_workflow_validations"
      )

    private def obscalcColumns(prefix: Option[String] = None): String =
      s"${obscalcMetaDataColumns(prefix)},\n${obscalcResultColumns(prefix)}"

    val SelectOne: Query[Observation.Id, Obscalc.Entry] =
      sql"""
        SELECT
          #${obscalcColumns()}
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
      """.query(obscalc_entry)

    def selectMany[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, Obscalc.Entry] =
      sql"""
        SELECT
          #${obscalcColumns()}
        FROM t_obscalc
        WHERE c_observation_id in ($enc)
      """.query(obscalc_entry)

    val SelectProgram: Query[Program.Id, Obscalc.Entry] =
      sql"""
        SELECT
          #${obscalcColumns("c".some)}
        FROM t_obscalc
        WHERE c_program_id = $program_id
      """.query(obscalc_entry)

    private def categorizedTimeColumns(prefix: String): String =
      prefixedColumns(prefix.some,
        "c_obscalc_state",
        "c_full_setup_time",
        "c_sci_obs_class",
        "c_sci_non_charged_time",
        "c_sci_program_time"
      )

    val full_categorized_time: Decoder[CategorizedTime] =
       (time_span *: obs_class *: time_span *: time_span).map: (setup, obsclass, nonCharged, program) =>
         CategorizedTime(
           ChargeClass.NonCharged -> nonCharged,
           ChargeClass.Program    -> program
         ).sumCharge(obsclass.chargeClass, setup)

    val SelectOneCategorizedTime: Query[Observation.Id, CalculatedValue[CategorizedTime]] =
      sql"""
        SELECT
          #${categorizedTimeColumns("c")}
        FROM t_obscalc c
        INNER JOIN t_observation o USING (c_observation_id)
        WHERE c.c_observation_id = $observation_id
          AND c.c_odb_error IS NULL
          AND o.c_workflow_user_state IS DISTINCT FROM 'inactive'
      """
        .query(calculation_state *: full_categorized_time.opt)
        .map: (state, catTime) =>
          CalculatedValue(state, catTime.getOrElse(CategorizedTime.Zero))

    val SelectProgramCategorizedTime: Query[Program.Id, (Observation.Id, CalculatedValue[CategorizedTime])] =
      sql"""
        SELECT
          c.c_observation_id,
          #${categorizedTimeColumns("c")}
        FROM t_obscalc c
        INNER JOIN t_observation o USING (c_observation_id)
        WHERE c.c_program_id = $program_id
          AND c.c_odb_error IS NULL
          AND o.c_workflow_user_state IS DISTINCT FROM 'inactive'
      """
        .query(observation_id *: calculation_state *: full_categorized_time.opt)
        .map: (oid, state, catTime) =>
          oid -> CalculatedValue(state, catTime.getOrElse(CategorizedTime.Zero))

    private def executionDigestColumns: String =
      prefixedColumns(none,
        "c_full_setup_time",
        "c_reacq_setup_time",

        "c_acq_obs_class",
        "c_acq_non_charged_time",
        "c_acq_program_time",
        "c_acq_offsets",
        "c_acq_offset_guide_states",
        "c_acq_atom_count",
        "c_acq_execution_state",

        "c_sci_obs_class",
        "c_sci_non_charged_time",
        "c_sci_program_time",
        "c_sci_offsets",
        "c_sci_offset_guide_states",
        "c_sci_atom_count",
        "c_sci_execution_state"
      )

    def selectManyExecutionDigest(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          c_observation_id,
          c_obscalc_state,
          c_odb_error,
          #${executionDigestColumns}
        FROM t_obscalc
        WHERE
      """(Void) |+|
        void"c_observation_id IN (" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")"

    val ResetCalculating: Command[Void] =
      sql"""
        UPDATE t_obscalc
        SET
          c_obscalc_state  = CASE
            WHEN t_obscalc.c_retry_at IS NULL THEN 'pending' :: e_calculation_state
            ELSE 'retry' :: e_calculation_state
          END
        WHERE c_obscalc_state = 'calculating'
      """.command

    val LoadPendingCalc: Query[Int, Obscalc.PendingCalc] =
      sql"""
        WITH tasks AS (
          SELECT c_program_id, c_observation_id
          FROM t_obscalc
          WHERE (
            c_obscalc_state = 'pending' OR
            (c_obscalc_state = 'retry' AND c_retry_at <= now())
          )
          ORDER BY c_last_invalidation LIMIT $int4
          FOR UPDATE SKIP LOCKED
        )
        UPDATE t_obscalc c
        SET c_obscalc_state = 'calculating'
        FROM tasks
        WHERE c.c_observation_id = tasks.c_observation_id
        RETURNING c.c_program_id, c.c_observation_id, c.c_last_invalidation
      """.query(pending_obscalc)

    val LoadPendingCalcFor: Query[Observation.Id, Obscalc.PendingCalc] =
      sql"""
        WITH task AS (
          SELECT c_program_id, c_observation_id
          FROM t_obscalc
          WHERE (
            c_obscalc_state = 'pending' OR
            (c_obscalc_state = 'retry' AND c_retry_at <= now())
          ) AND c_observation_id = $observation_id
          FOR UPDATE SKIP LOCKED
        )
        UPDATE t_obscalc c
        SET c_obscalc_state = 'calculating'
        FROM task
        WHERE c.c_observation_id = task.c_observation_id
        RETURNING c.c_program_id, c.c_observation_id, c.c_last_invalidation
      """.query(pending_obscalc)

    private def updatesForResult(r: Obscalc.Result): NonEmptyList[AppliedFragment] =
      NonEmptyList.of(
        sql"c_odb_error            = ${odb_error.opt}"(r.odbError),

        // Imaging ITC Results
        sql"c_img_target_id        = ${target_id.opt}"(r.itcResult.map(_.imaging.targetId)),
        sql"c_img_exposure_time    = ${time_span.opt}"(r.itcResult.map(_.imaging.value.exposureTime)),
        sql"c_img_exposure_count   = ${int4_pos.opt}"(r.itcResult.map(_.imaging.value.exposureCount)),
        sql"c_img_wavelength       = ${wavelength_pm.opt}"(r.itcResult.flatMap(_.imaging.signalToNoise).map(_.wavelength)),
        sql"c_img_single_sn        = ${signal_to_noise.opt}"(r.itcResult.flatMap(_.imaging.signalToNoise).map(_.single.value)),
        sql"c_img_total_sn         = ${signal_to_noise.opt}"(r.itcResult.flatMap(_.imaging.signalToNoise).map(_.total.value)),

        // Spectroscopy ITC Results
        sql"c_spec_target_id       = ${target_id.opt}"(r.itcResult.map(_.spectroscopy.targetId)),
        sql"c_spec_exposure_time   = ${time_span.opt}"(r.itcResult.map(_.spectroscopy.value.exposureTime)),
        sql"c_spec_exposure_count  = ${int4_pos.opt}"(r.itcResult.map(_.spectroscopy.value.exposureCount)),
        sql"c_spec_wavelength      = ${wavelength_pm.opt}"(r.itcResult.flatMap(_.spectroscopy.signalToNoise).map(_.wavelength)),
        sql"c_spec_single_sn       = ${signal_to_noise.opt}"(r.itcResult.flatMap(_.spectroscopy.signalToNoise).map(_.single.value)),
        sql"c_spec_total_sn        = ${signal_to_noise.opt}"(r.itcResult.flatMap(_.spectroscopy.signalToNoise).map(_.total.value)),

        // Setup Times
        sql"c_full_setup_time      = ${time_span.opt}"(r.digest.map(_.setup.full)),
        sql"c_reacq_setup_time     = ${time_span.opt}"(r.digest.map(_.setup.reacquisition)),

        // Acquisition Digest
        sql"c_acq_obs_class           = ${obs_class.opt}"(r.digest.map(_.acquisition.observeClass)),
        sql"c_acq_non_charged_time    = ${time_span.opt}"(r.digest.map(_.acquisition.timeEstimate(ChargeClass.NonCharged))),
        sql"c_acq_program_time        = ${time_span.opt}"(r.digest.map(_.acquisition.timeEstimate(ChargeClass.Program))),
        sql"c_acq_offsets             = ${_offset_array.opt}"(r.digest.map(_.acquisition.configs.toList.map(_.offset))),
        sql"c_acq_offset_guide_states = ${_guide_state.opt}"(r.digest.map(_.acquisition.configs.toList.map(_.guiding))),
        sql"c_acq_atom_count          = ${int4_nonneg.opt}"(r.digest.map(_.acquisition.atomCount)),
        sql"c_acq_execution_state     = ${execution_state.opt}"(r.digest.map(_.acquisition.executionState)),

        // Science Digest
        sql"c_sci_obs_class           = ${obs_class.opt}"(r.digest.map(_.science.observeClass)),
        sql"c_sci_non_charged_time    = ${time_span.opt}"(r.digest.map(_.science.timeEstimate(ChargeClass.NonCharged))),
        sql"c_sci_program_time        = ${time_span.opt}"(r.digest.map(_.science.timeEstimate(ChargeClass.Program))),
        sql"c_sci_offsets             = ${_offset_array.opt}"(r.digest.map(_.science.configs.toList.map(_.offset))),
        sql"c_sci_offset_guide_states = ${_guide_state.opt}"(r.digest.map(_.science.configs.toList.map(_.guiding))),
        sql"c_sci_atom_count          = ${int4_nonneg.opt}"(r.digest.map(_.science.atomCount)),
        sql"c_sci_execution_state     = ${execution_state.opt}"(r.digest.map(_.science.executionState)),

        // Workflow
        sql"c_workflow_state       = ${observation_workflow_state}"(r.workflow.state),
        sql"c_workflow_transitions = ${_observation_workflow_state}"(r.workflow.validTransitions),
        sql"c_workflow_validations = ${_observation_validation}"(r.workflow.validationErrors)
      )

    val SelectLastInvalidationForUpdate: Query[Observation.Id, Timestamp] =
      sql"""
        SELECT c_last_invalidation
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
        FOR UPDATE
      """.query(core_timestamp)

    def storeResult(
      pending:  Obscalc.PendingCalc,
      result:   Obscalc.Result,
      newState: CalculationState
    ): AppliedFragment =

      val isRetry = newState === CalculationState.Retry
      val upState        =  sql"c_obscalc_state = $calculation_state"(newState)
      val upLastUpdate   = void"c_last_update   = now()"
      val upFailureCount = void"c_failure_count = " |+|
                           (if isRetry then void"c_failure_count + 1" else void"0")
      val upRetryAt      = void"c_retry_at      = " |+|
                           (if isRetry then void"now() + (interval '1 minute' * POWER(2, LEAST(c_failure_count, 5)))" else void"NULL")

      val updates = upState :: upLastUpdate :: upFailureCount :: upRetryAt :: updatesForResult(result)

      void"UPDATE t_obscalc " |+|
        void"SET " |+| updates.intercalate(void", ") |+| void" " |+|
        sql"WHERE c_observation_id = $observation_id"(pending.observationId) |+| void" " |+|
        void"RETURNING c_program_id, c_observation_id, c_obscalc_state, c_last_invalidation, c_last_update, c_retry_at, c_failure_count"
