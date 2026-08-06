// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.TooTrigger
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait TooTriggerService[F[_]]:

  /**
   * Creates a new ToO trigger (in the 'requested' state) for the single observation
   * selected by `which` (already scoped to observations the caller may read).
   * Returns the new id, or a failure if the selection is empty (observation not
   * found or not accessible) or an active request already exists.
   */
  def request(which: AppliedFragment)(using Transaction[F]): F[Result[TooTrigger.Id]]

  /**
   * Withdraws the trigger selected by `which` (already scoped to the caller's
   * writable programs), provided it is in a withdrawable state: 'requested', or
   * 'accepted' while its observation is not yet 'ongoing'.  Returns the id if it
   * was withdrawn, or None if no eligible trigger matched.
   */
  def withdraw(which: AppliedFragment, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]]

  /** Accepts a 'requested' trigger.  Staff-gated by the caller. */
  def accept(id: TooTrigger.Id)(using Transaction[F]): F[Option[TooTrigger.Id]]

  /** Denies a 'requested' trigger.  Staff-gated by the caller. */
  def deny(id: TooTrigger.Id, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]]

object TooTriggerService:

  def instantiate[F[_]: {Concurrent, Tracer as T, Services}]: TooTriggerService[F] =

    new TooTriggerService[F]:

      override def request(which: AppliedFragment)(using Transaction[F]): F[Result[TooTrigger.Id]] =
        T.span("requestTooTrigger").surround:
          val af = Statements.request(which)
          session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
            session.prepareR(af.fragment.query(too_trigger_id)).use(_.option(af.argument)).map:
              case Some(t) => Result(t)
              case None    => OdbError.InvalidArgument(Some("Observation not found or not accessible.")).asFailure
          .recover:
            case SqlState.UniqueViolation(_) =>
              OdbError.InvalidArgument(Some("This observation already has an active (requested) ToO trigger.")).asFailure

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

    // Withdrawable if 'requested', or 'accepted' while the observation is not yet
    // 'ongoing' (workflow state is cached on t_obscalc).
    def withdraw(which: AppliedFragment, reason: Option[NonEmptyString]): AppliedFragment =
      void"""
        UPDATE t_too_trigger t
        SET c_status = 'withdrawn',
            c_resolution_reason = """ |+| sql"${text_nonempty.opt}"(reason) |+|
      void"""
        WHERE t.c_too_trigger_id IN (""" |+| which |+| void""")
          AND (
            t.c_status = 'requested'
            OR (
              t.c_status = 'accepted'
              AND NOT EXISTS (
                SELECT 1 FROM t_obscalc oc
                WHERE oc.c_observation_id = t.c_observation_id
                  AND oc.c_workflow_state = 'ongoing'
              )
            )
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
