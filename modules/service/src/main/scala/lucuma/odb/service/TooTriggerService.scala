// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.TooTrigger
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait TooTriggerService[F[_]]:

  /**
   * Declines the trigger selected by `which` (already scoped to the caller's
   * writable programs), provided it is still 'requested'.  Staff-gated by the
   * caller.
   *
   * Declining both records the observer's decision and clears the observation's
   * `Ready` user state, so the observation returns to `Defined`.  Those happen
   * in that order on purpose: the status moves out of 'requested' first, so the
   * database trigger watching the user state finds nothing to withdraw and the
   * decision -- with its reason -- is what survives in the history.
   *
   * Returns the id if it was declined, or None if no eligible trigger matched.
   */
  def decline(which: AppliedFragment, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]]

object TooTriggerService:

  def instantiate[F[_]: {Concurrent, Tracer as T, Services}]: TooTriggerService[F] =

    new TooTriggerService[F]:

      override def decline(which: AppliedFragment, reason: Option[NonEmptyString])(using Transaction[F]): F[Option[TooTrigger.Id]] =
        T.span("declineTooTrigger").surround:
          val af = Statements.decline(which, reason)
          session.prepareR(af.fragment.query(too_trigger_id *: observation_id)).use(_.option(af.argument)).flatMap:
            case None            => none[TooTrigger.Id].pure[F]
            case Some((tid, oid)) =>
              session.execute(Statements.ClearReady)(oid).as(tid.some)

  object Statements:

    def decline(which: AppliedFragment, reason: Option[NonEmptyString]): AppliedFragment =
      void"""
        UPDATE t_too_trigger t
        SET c_status = 'declined',
            c_resolution_reason = """ |+| sql"${text_nonempty.opt}"(reason) |+|
      void"""
        WHERE t.c_too_trigger_id IN (""" |+| which |+| void""")
          AND t.c_status = 'requested'
        RETURNING t.c_too_trigger_id, t.c_observation_id
      """

    // Only clears a 'ready' user state; an observation that has since been made
    // inactive keeps that, and one already executing is unaffected either way.
    val ClearReady: Command[lucuma.core.model.Observation.Id] =
      sql"""
        UPDATE t_observation
        SET c_workflow_user_state = NULL
        WHERE c_observation_id = $observation_id
          AND c_workflow_user_state = 'ready'
      """.command
