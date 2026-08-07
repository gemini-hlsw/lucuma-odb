// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.TooActivation
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.TooTrigger
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.codec.boolean.*
import skunk.implicits.*

import Services.Syntax.*

trait TooTriggerService[F[_]]:

  /**
   * Creates a new ToO trigger (in the 'requested' state) for the single observation
   * selected by `which` (already scoped to observations the caller may read).
   *
   * Fails unless the observation is one that can meaningfully be triggered: it
   * declares a ToO activation other than NONE, its asterism no longer holds an
   * opportunity placeholder, and its workflow state is exactly `Defined` -- no
   * user state, no validation errors, no unapproved configuration.  Each of
   * those produces its own message.  A live ('requested' or 'accepted') trigger
   * blocks a second one; 'denied' and 'withdrawn' attempts do not.
   */
  def request(which: AppliedFragment)(using Transaction[F]): F[Result[TooTrigger.Id]]

  /**
   * Withdraws the trigger selected by `which` (already scoped to the caller's
   * writable programs), provided it is 'requested' or 'accepted' and its
   * observation has not begun executing.  Acceptance is not the point of no
   * return -- the first execution event is.  Returns the id if it was withdrawn,
   * or None if no eligible trigger matched.
   */
  def withdraw(which: AppliedFragment, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]]

  /** Accepts a 'requested' trigger.  Staff-gated by the caller. */
  def accept(id: TooTrigger.Id)(using Transaction[F]): F[Option[TooTrigger.Id]]

  /** Denies a 'requested' trigger.  Staff-gated by the caller. */
  def deny(id: TooTrigger.Id, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]]

object TooTriggerService:

  def instantiate[F[_]: {Concurrent, Tracer as T, Services}]: TooTriggerService[F] =

    new TooTriggerService[F]:

      // Inserts the trigger, once the preconditions are known to hold.  The
      // unique index is what actually rules out a concurrent second request, so
      // the UniqueViolation recovery stays even though `request` has just
      // established that the observation is Defined (and therefore untriggered).
      private def insert(which: AppliedFragment): F[Result[TooTrigger.Id]] =
        val af = Statements.request(which)
        session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
          session.prepareR(af.fragment.query(too_trigger_id)).use(_.option(af.argument)).map:
            case Some(t) => Result(t)
            case None    => OdbError.InvalidArgument(Some("Observation not found or not accessible.")).asFailure
        .recover:
          case SqlState.UniqueViolation(_) =>
            OdbError.InvalidArgument(Some("This observation already has a live (requested or accepted) ToO trigger.")).asFailure

      override def request(which: AppliedFragment)(using Transaction[F]): F[Result[TooTrigger.Id]] =
        T.span("requestTooTrigger").surround:
          val af = Statements.preconditions(which)
          session
            .prepareR(af.fragment.query(too_activation *: bool *: observation_workflow_state))
            .use(_.option(af.argument))
            .flatMap:
              case None =>
                OdbError.InvalidArgument(Some("Observation not found or not accessible.")).asFailureF

              case Some((activation, hasPlaceholder, state)) =>
                if activation === TooActivation.None then
                  OdbError.InvalidArgument(Some(
                    "This observation is not a Target of Opportunity; set its ToO activation before requesting a trigger."
                  )).asFailureF

                else if hasPlaceholder then
                  OdbError.InvalidArgument(Some(
                    "This observation still has a Target of Opportunity placeholder; replace it with the actual target coordinates before requesting a trigger."
                  )).asFailureF

                else if state =!= ObservationWorkflowState.Defined then
                  OdbError.InvalidArgument(Some(
                    s"A ToO trigger may only be requested for an observation in the Defined state, but this one is ${state.tag.toScreamingSnakeCase}."
                  )).asFailureF

                else insert(which)

      override def withdraw(which: AppliedFragment, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]] =
        T.span("withdrawTooTrigger").surround:
          val af = Statements.withdraw(which, reason)
          session.prepareR(af.fragment.query(too_trigger_id)).use(_.option(af.argument))

      override def accept(id: TooTrigger.Id)(using Transaction[F]): F[Option[TooTrigger.Id]] =
        T.span("acceptTooTrigger").surround:
          session.option(Statements.Accept)(id)

      override def deny(id: TooTrigger.Id, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]] =
        T.span("denyTooTrigger").surround:
          session.option(Statements.Deny)(id, reason)

  object Statements:

    // The three things that make an observation triggerable, read in one pass so
    // each can be reported separately.  The workflow state is the value obscalc
    // cached rather than a fresh computation: it is a single indexed read, it is
    // the same value the UI is displaying, and it is only a guard -- an accepted
    // trigger cannot make an observation Ready on its own, because the workflow
    // recomputes validation at that point regardless of what was cached here.
    //
    // t_obscalc rows are created lazily, so an observation that has never been
    // calculated reads as 'undefined' and is refused, which is the honest answer.
    def preconditions(which: AppliedFragment): AppliedFragment =
      void"""
        SELECT
          o.c_too_activation,
          EXISTS (
            SELECT 1
            FROM t_asterism_target a
            JOIN t_target t ON t.c_target_id = a.c_target_id
            WHERE a.c_observation_id = o.c_observation_id
              AND t.c_existence = 'present'
              AND t.c_type = 'opportunity'
          ),
          COALESCE(c.c_workflow_state, 'undefined'::e_workflow_state)
        FROM t_observation o
        LEFT JOIN t_obscalc c ON c.c_observation_id = o.c_observation_id
        WHERE o.c_observation_id IN (""" |+| which |+| void""")
      """

    // The program is derived from the observation, keeping the (program, observation)
    // pair consistent with t_observation (and satisfying the composite foreign key).
    // Status, requestedAt, updatedAt and requestedBy all fall to their column defaults.
    def request(which: AppliedFragment): AppliedFragment =
      void"""
        INSERT INTO t_too_trigger (c_observation_id, c_program_id)
        SELECT o.c_observation_id, o.c_program_id
        FROM t_observation o
        WHERE o.c_observation_id IN (""" |+| which |+| void""")
        RETURNING c_too_trigger_id
      """

    // Withdrawable while 'requested' or 'accepted', up until the observation
    // begins executing.  Acceptance is not the point of no return; the first
    // execution event is.
    //
    // "Execution has begun" is tested against t_execution_event directly rather
    // than against the t_obscalc cache this used to consult: that cache is
    // refreshed asynchronously, so it left a window in which execution had
    // started but withdrawal still succeeded.  The two conditions mirror the
    // 'not_started' arm of v_generator_params.c_execution_state, which is the
    // canonical definition -- a slew alone does not count as execution, and a
    // declared state overrides the events.  Spelled out here rather than joined
    // from the view, which is wide, per-target, and about generator input.
    def withdraw(which: AppliedFragment, reason: Option[NonEmptyString]): AppliedFragment =
      void"""
        UPDATE t_too_trigger t
        SET c_status = 'withdrawn',
            c_resolution_reason = """ |+| sql"${text_nonempty.opt}"(reason) |+|
      void"""
        WHERE t.c_too_trigger_id IN (""" |+| which |+| void""")
          AND t.c_status IN ('requested', 'accepted')
          AND NOT EXISTS (
            SELECT 1 FROM t_execution_event e
            WHERE e.c_observation_id = t.c_observation_id
              AND e.c_event_type <> 'slew'::e_execution_event_type
          )
          AND NOT EXISTS (
            SELECT 1 FROM t_observation o
            WHERE o.c_observation_id = t.c_observation_id
              AND o.c_declared_state IS NOT NULL
          )
        RETURNING t.c_too_trigger_id
      """

    val Accept: Query[TooTrigger.Id, TooTrigger.Id] =
      sql"""
        UPDATE t_too_trigger
        SET c_status = 'accepted'
        WHERE c_too_trigger_id = $too_trigger_id
          AND c_status = 'requested'
        RETURNING c_too_trigger_id
      """.query(too_trigger_id)

    val Deny: Query[(TooTrigger.Id, Option[NonEmptyString]), TooTrigger.Id] =
      sql"""
        UPDATE t_too_trigger
        SET c_status = 'denied',
            c_resolution_reason = ${text_nonempty.opt}
        WHERE c_too_trigger_id = $too_trigger_id
          AND c_status = 'requested'
        RETURNING c_too_trigger_id
      """.query(too_trigger_id).contramap: (id, reason) =>
        (reason, id)
