// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.Partner
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.TacFeedbackInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

trait TacFeedbackService[F[_]]:

  /**
   * Sets a partner's TAC feedback for a program, which must have a proposal in
   * which the partner has a non-zero time split.  Existing feedback is updated,
   * keeping the current value of any field that is not specified, while an
   * explicit null comment removes the comments.  New feedback requires every
   * field except the comments.
   *
   * @return the program id and partner identifying the feedback
   */
  def setTacFeedback(
    input: AccessControl.CheckedWithId[TacFeedbackInput, Program.Id]
  )(using Transaction[F]): F[Result[(Program.Id, Partner)]]

object TacFeedbackService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): TacFeedbackService[F] =
    new TacFeedbackService[F]:

      override def setTacFeedback(
        input: AccessControl.CheckedWithId[TacFeedbackInput, Program.Id]
      )(using Transaction[F]): F[Result[(Program.Id, Partner)]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (feedback, pid) =>
          proposalService.hasProposal(pid).flatMap: hasProposal =>
            if !hasProposal then
              OdbError.InvalidArgument(s"Program $pid has no proposal, so TAC feedback cannot be set.".some).asFailureF
            else
              session.unique(Statements.HasNonZeroSplit)((pid, feedback.partner)).flatMap: hasSplit =>
                if hasSplit then upsert(pid, feedback).map(_.as((pid, feedback.partner)))
                else OdbError.InvalidArgument(
                  s"Partner ${feedback.partner.tag.toUpperCase} has no time split in the proposal for program $pid, so it cannot provide TAC feedback.".some
                ).asFailureF

      private def upsert(pid: Program.Id, f: TacFeedbackInput): F[Result[Unit]] =
        // With nothing to change, there is no need to touch the row.
        val noChanges = f.decision.isEmpty && f.ranking.isEmpty && f.recommendedTime.isEmpty && f.comments.isAbsent
        if noChanges then
          session
            .unique(Statements.TacFeedbackExists)((pid, f.partner))
            .map(exists => if exists then Result.unit else missingFields(f))
        else
          session
            .option(Statements.UpdateTacFeedback)((f.decision, f.ranking, f.recommendedTime, f.comments.isDefined, f.comments.toOption, pid, f.partner))
            .flatMap:
              case Some(_) => Result.unit.pure[F]
              case None    => insert(pid, f)

      private def insert(pid: Program.Id, f: TacFeedbackInput): F[Result[Unit]] =
        (f.decision, f.ranking, f.recommendedTime)
          .tupled
          .fold(missingFields(f).pure[F]): (decision, ranking, time) =>
            session
              .execute(Statements.InsertTacFeedback)((pid, f.partner, decision, ranking, time, f.comments.toOption))
              .as(Result.unit)

      private def missingFields(f: TacFeedbackInput): Result[Unit] =
        val missing = List(
          Option.when(f.decision.isEmpty)("decision"),
          Option.when(f.ranking.isEmpty)("ranking"),
          Option.when(f.recommendedTime.isEmpty)("recommendedTime")
        ).flatten
        OdbError.InvalidArgument(
          s"${missing.mkString(", ")} must be specified when creating TAC feedback for partner ${f.partner.tag.toUpperCase}.".some
        ).asFailure

  object Statements:

    val HasNonZeroSplit: Query[(Program.Id, Partner), Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
            FROM t_partner_split
           WHERE c_program_id = $program_id
             AND c_partner    = $partner
             AND c_percent    > 0
        )
      """.query(bool)

    val TacFeedbackExists: Query[(Program.Id, Partner), Boolean] =
      sql"""
        SELECT EXISTS (
          SELECT 1
            FROM t_tac_feedback
           WHERE c_program_id = $program_id
             AND c_partner    = $partner
        )
      """.query(bool)

    // Comments are set (possibly to NULL) only when the Boolean flag is true.
    val UpdateTacFeedback: Query[(Option[TacPartnerDecision], Option[PosBigDecimal], Option[TimeSpan], Boolean, Option[NonEmptyString], Program.Id, Partner), Partner] =
      sql"""
        UPDATE t_tac_feedback
           SET c_decision         = COALESCE(${tac_partner_decision.opt}, c_decision),
               c_ranking          = COALESCE(${numeric_pos.opt}, c_ranking),
               c_recommended_time = COALESCE(${time_span.opt}, c_recommended_time),
               c_comments         = CASE WHEN $bool THEN ${text_nonempty.opt} ELSE c_comments END
         WHERE c_program_id = $program_id
           AND c_partner    = $partner
     RETURNING c_partner
      """.query(partner)

    // ON CONFLICT covers a concurrent insert of the same program and partner.
    val InsertTacFeedback: Command[(Program.Id, Partner, TacPartnerDecision, PosBigDecimal, TimeSpan, Option[NonEmptyString])] =
      sql"""
        INSERT INTO t_tac_feedback (
          c_program_id,
          c_partner,
          c_decision,
          c_ranking,
          c_recommended_time,
          c_comments
        ) VALUES (
          $program_id,
          $partner,
          $tac_partner_decision,
          $numeric_pos,
          $time_span,
          ${text_nonempty.opt}
        )
        ON CONFLICT (c_program_id, c_partner) DO UPDATE SET
          c_decision         = EXCLUDED.c_decision,
          c_ranking          = EXCLUDED.c_ranking,
          c_recommended_time = EXCLUDED.c_recommended_time,
          c_comments         = EXCLUDED.c_comments
      """.command
