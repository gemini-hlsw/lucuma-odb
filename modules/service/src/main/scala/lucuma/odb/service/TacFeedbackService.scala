// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.TacFeedbackInput
import lucuma.odb.graphql.input.TacRecommendedTimeInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

trait TacFeedbackService[F[_]]:

  /**
   * Sets the TAC feedback of a time accounting category for a program, whose
   * submitted proposal the category must be eligible to provide feedback for
   * (see `TacFeedbackService.eligibility`).  Existing feedback is updated,
   * keeping the current value of any field that is not specified, while an
   * explicit null comment removes the comments.  Supplied recommended times
   * replace all existing ones.  New feedback requires a decision and ranking.
   *
   * Recommended times are kept with the program's allocations.  Setting one
   * also sets the allocated duration to the same time, and removing one
   * removes the allocation.  Observation bands are left alone: they follow
   * the allocations once the proposal is accepted.
   *
   * @return the program id and time accounting category identifying the feedback
   */
  def setTacFeedback(
    input: AccessControl.CheckedWithId[TacFeedbackInput, Program.Id]
  )(using Transaction[F]): F[Result[(Program.Id, TimeAccountingCategory)]]

  /**
   * Deletes the TAC feedback of a time accounting category for a program,
   * along with the allocations that carry its recommended times.  The program
   * must have a submitted proposal.
   *
   * @return `true` if feedback was deleted, `false` if there was none
   */
  def deleteTacFeedback(
    input: AccessControl.CheckedWithId[TimeAccountingCategory, Program.Id]
  )(using Transaction[F]): F[Result[Boolean]]

  /**
   * Deletes the TAC feedback of every time accounting category for a program,
   * along with the allocations that carry recommended times.  Intended for use
   * by `ProposalService` when the proposal is deleted.
   */
  def deleteAllTacFeedback(pid: Program.Id)(using Transaction[F], Services.SuperUserAccess): F[Unit]

object TacFeedbackService:

  /**
   * What decides whether a time accounting category may provide TAC feedback
   * for a proposal.
   *
   * @param status            the proposal status
   * @param cfpType           the type of the proposal's call for proposals, if any
   * @param piPartner         the Gemini partner the PI is affiliated with, if any
   * @param exchangePartner   the exchange partner on whose behalf the proposal requests time, if any
   * @param splitPartners     the partners with a non-zero time split
   */
  case class FeedbackContext(
    status:            ProposalStatus,
    cfpType:           Option[GeminiCallForProposalsType],
    piPartner:         Option[Partner],
    exchangePartner:   Option[ExchangePartner],
    splitPartners:     Set[Partner]
  )

  /**
   * Checks whether a time accounting category may change its TAC feedback for
   * a proposal.  Feedback may only change while the proposal is submitted.
   * Deletion needs nothing more, so that feedback which is no longer eligible
   * can be removed.  Otherwise, a partner's category requires a non-zero time
   * split for the partner or, for a Fast Turnaround or Poor Weather proposal,
   * a PI affiliated with it.  The remaining categories each have their own
   * rule.
   *
   * @return the reason the feedback may not change, if it may not
   */
  def eligibility(
    pid:       Program.Id,
    category:  TimeAccountingCategory,
    ctx:       FeedbackContext,
    forDelete: Boolean
  ): Either[String, Unit] =
    import TimeAccountingCategory.*

    val name = category.tag.toUpperCase

    def require(cond: Boolean, reason: => String): Either[String, Unit] =
      Either.cond(cond, (), reason)

    def requireExchangePartner(p: ExchangePartner, community: String): Either[String, Unit] =
      require(
        ctx.exchangePartner.contains(p),
        s"Time accounting category $name can only provide TAC feedback for a proposal requesting time on behalf of $community, which is not the case for program $pid."
      )

    def requireCfpType(t: GeminiCallForProposalsType): Either[String, Unit] =
      require(
        ctx.cfpType.contains(t),
        s"Time accounting category $name can only provide TAC feedback for a proposal to a ${t.title} call for proposals, which is not the case for program $pid."
      )

    val categoryRule: Either[String, Unit] =
      category match
        case AR | BR | CA | CL | KR | UH | US =>
          // Fast Turnaround and Poor Weather proposals have no splits: their
          // time comes from the PI's partner.
          val piPartnerCall: Boolean =
            ctx.cfpType.exists(t => t === GeminiCallForProposalsType.FastTurnaround || t === GeminiCallForProposalsType.PoorWeather)
          require(
            Partner.values.find(_.timeAccountingCategory === category).exists: p =>
              ctx.splitPartners.contains(p) || (piPartnerCall && ctx.piPartner.contains(p)),
            s"Partner $name has no time split in the proposal for program $pid, nor is it the PI's partner in a Fast Turnaround or Poor Weather proposal, so it cannot provide TAC feedback."
          )
        case CAL | CFHT | ENG => s"Time accounting category $name cannot provide TAC feedback.".asLeft
        case GT | LTP         => ().asRight
        case JP               => requireExchangePartner(ExchangePartner.Subaru, "Subaru")
        case KECK             => requireExchangePartner(ExchangePartner.Keck, "Keck")
        case DD               => requireCfpType(GeminiCallForProposalsType.DirectorsTime)
        case DS               => requireCfpType(GeminiCallForProposalsType.DemoScience)
        case LP               => requireCfpType(GeminiCallForProposalsType.LargeProgram)
        case SV               => requireCfpType(GeminiCallForProposalsType.SystemVerification)

    require(
      ctx.status === ProposalStatus.Submitted,
      s"TAC feedback for program $pid cannot be changed while its proposal status is ${ctx.status.tag.toScreamingSnakeCase}."
    ) *> (if forDelete then ().asRight else categoryRule)

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): TacFeedbackService[F] =
    new TacFeedbackService[F]:

      override def setTacFeedback(
        input: AccessControl.CheckedWithId[TacFeedbackInput, Program.Id]
      )(using Transaction[F]): F[Result[(Program.Id, TimeAccountingCategory)]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (feedback, pid) =>
          checkEligibility(pid, feedback.timeAccountingCategory, forDelete = false).flatMap: r =>
            r.flatTraverse: _ =>
              upsert(pid, feedback).map(_.as((pid, feedback.timeAccountingCategory)))

      override def deleteTacFeedback(
        input: AccessControl.CheckedWithId[TimeAccountingCategory, Program.Id]
      )(using Transaction[F]): F[Result[Boolean]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (category, pid) =>
          checkEligibility(pid, category, forDelete = true).flatMap: r =>
            r.flatTraverse: _ =>
              for
                deleted <- session.option(Statements.DeleteTacFeedback)((pid, category))
                _       <- session.execute(Statements.DeleteRecommendedAllocations)((pid, category))
              yield Result(deleted.isDefined)

      override def deleteAllTacFeedback(pid: Program.Id)(using Transaction[F], Services.SuperUserAccess): F[Unit] =
        session.execute(Statements.DeleteAllTacFeedback)(pid) *>
          session.execute(Statements.DeleteAllRecommendedAllocations)(pid).void

      private def checkEligibility(
        pid:       Program.Id,
        category:  TimeAccountingCategory,
        forDelete: Boolean
      ): F[Result[Unit]] =
        session.option(Statements.SelectFeedbackContext)(pid).map:
          case None      =>
            OdbError.InvalidArgument(s"Program $pid has no proposal, so its TAC feedback cannot be changed.".some).asFailure
          case Some(ctx) =>
            eligibility(pid, category, ctx, forDelete).fold(
              reason => OdbError.InvalidArgument(reason.some).asFailure,
              _      => Result.unit
            )

      private def upsert(pid: Program.Id, f: TacFeedbackInput): F[Result[Unit]] =
        upsertFeedback(pid, f).flatMap: r =>
          r.flatTraverse: _ =>
            f.recommendedTimes
             .traverse_(replaceRecommendedTimes(pid, f.timeAccountingCategory, _))
             .as(Result.unit)

      private def upsertFeedback(pid: Program.Id, f: TacFeedbackInput): F[Result[Unit]] =
        // When nothing in the feedback row itself changes, there is no need to
        // update it, but it must already exist.  Locking it serializes the
        // replacement of its recommended times with other edits and deletion,
        // just as the UPDATE below does.
        val noFeedbackChanges = f.decision.isEmpty && f.ranking.isEmpty && f.comments.isAbsent
        if noFeedbackChanges then
          session
            .option(Statements.LockTacFeedback)((pid, f.timeAccountingCategory))
            .map(_.fold(missingFields(f))(_ => Result.unit))
        else
          session
            .option(Statements.UpdateTacFeedback)((f.decision, f.ranking, f.comments.isDefined, f.comments.toOption, pid, f.timeAccountingCategory))
            .flatMap:
              case Some(_) => Result.unit.pure[F]
              case None    => insert(pid, f)

      private def insert(pid: Program.Id, f: TacFeedbackInput): F[Result[Unit]] =
        (f.decision, f.ranking)
          .tupled
          .fold(missingFields(f).pure[F]): (decision, ranking) =>
            session
              .execute(Statements.InsertTacFeedback)((pid, f.timeAccountingCategory, decision, ranking, f.comments.toOption, f.comments.isDefined))
              .as(Result.unit)

      // Replaces the recommended times, touching only the rows that change so
      // that an identical replacement produces no program edit event.
      private def replaceRecommendedTimes(
        pid:      Program.Id,
        category: TimeAccountingCategory,
        times:    List[TacRecommendedTimeInput]
      ): F[Unit] =
        session.execute(Statements.DeleteOtherRecommendedAllocations)((pid, category, times.map(_.band))) *>
          NonEmptyList.fromList(times).traverse_ : nel =>
            val rows = nel.toList.map(t => (pid, category, t.band, t.time, t.time))
            session.execute(Statements.upsertRecommendedAllocations(rows.length))(rows)

      private def missingFields(f: TacFeedbackInput): Result[Unit] =
        val missing = List(
          Option.when(f.decision.isEmpty)("decision"),
          Option.when(f.ranking.isEmpty)("ranking")
        ).flatten
        OdbError.InvalidArgument(
          s"${missing.mkString(", ")} must be specified when creating TAC feedback for time accounting category ${f.timeAccountingCategory.tag.toUpperCase}.".some
        ).asFailure

  object Statements:

    private val feedback_context: Decoder[FeedbackContext] =
      (proposal_status *: gemini_proposal_type.opt *: partner.opt *: exchange_partner.opt *: _partner.map(_.toList.toSet))
        .to[FeedbackContext]

    // No row when the program has no proposal.  The program row is locked so
    // that its proposal status cannot change before the feedback is written.
    val SelectFeedbackContext: Query[Program.Id, FeedbackContext] =
      sql"""
        SELECT prog.c_proposal_status,
               cfp.c_gemini_proposal_type,
               pi.c_gemini_partner,
               prop.c_exchange_partner,
               COALESCE(
                 (SELECT ARRAY_AGG(c_partner) FROM t_partner_split WHERE c_program_id = prog.c_program_id AND c_percent > 0),
                 '{}'
               )
          FROM t_proposal prop
          JOIN t_program prog
            ON prog.c_program_id = prop.c_program_id
          LEFT JOIN t_cfp cfp
            ON cfp.c_cfp_id = prop.c_cfp_id
          LEFT JOIN t_program_user pi
            ON pi.c_program_id = prog.c_program_id
           AND pi.c_role = 'pi'
         WHERE prop.c_program_id = $program_id
           FOR SHARE OF prog
      """.query(feedback_context)

    val DeleteTacFeedback: Query[(Program.Id, TimeAccountingCategory), TimeAccountingCategory] =
      sql"""
        DELETE FROM t_tac_feedback
         WHERE c_program_id  = $program_id
           AND c_ta_category = $time_accounting_category
     RETURNING c_ta_category
      """.query(time_accounting_category)

    val DeleteAllTacFeedback: Command[Program.Id] =
      sql"""
        DELETE FROM t_tac_feedback
         WHERE c_program_id = $program_id
      """.command

    // Removes the allocations that carry a recommended time for the category.
    val DeleteRecommendedAllocations: Command[(Program.Id, TimeAccountingCategory)] =
      sql"""
        DELETE FROM t_allocation
         WHERE c_program_id  = $program_id
           AND c_ta_category = $time_accounting_category
           AND c_recommended_time IS NOT NULL
      """.command

    // Removes the allocations that carry a recommended time for any category.
    val DeleteAllRecommendedAllocations: Command[Program.Id] =
      sql"""
        DELETE FROM t_allocation
         WHERE c_program_id = $program_id
           AND c_recommended_time IS NOT NULL
      """.command

    val LockTacFeedback: Query[(Program.Id, TimeAccountingCategory), TimeAccountingCategory] =
      sql"""
        SELECT c_ta_category
          FROM t_tac_feedback
         WHERE c_program_id  = $program_id
           AND c_ta_category = $time_accounting_category
           FOR UPDATE
      """.query(time_accounting_category)

    // Comments are set (possibly to NULL) only when the Boolean flag is true.
    val UpdateTacFeedback: Query[(Option[TacPartnerDecision], Option[PosBigDecimal], Boolean, Option[NonEmptyString], Program.Id, TimeAccountingCategory), TimeAccountingCategory] =
      sql"""
        UPDATE t_tac_feedback
           SET c_decision = COALESCE(${tac_partner_decision.opt}, c_decision),
               c_ranking  = COALESCE(${numeric_pos.opt}, c_ranking),
               c_comments = CASE WHEN $bool THEN ${text_nonempty.opt} ELSE c_comments END
         WHERE c_program_id  = $program_id
           AND c_ta_category = $time_accounting_category
     RETURNING c_ta_category
      """.query(time_accounting_category)

    // ON CONFLICT covers a concurrent insert of the same program and category.
    // As in an update, the comments are replaced only when the Boolean flag is
    // true, so omitted comments keep the value the other insert gave them.
    val InsertTacFeedback: Command[(Program.Id, TimeAccountingCategory, TacPartnerDecision, PosBigDecimal, Option[NonEmptyString], Boolean)] =
      sql"""
        INSERT INTO t_tac_feedback AS t (
          c_program_id,
          c_ta_category,
          c_decision,
          c_ranking,
          c_comments
        ) VALUES (
          $program_id,
          $time_accounting_category,
          $tac_partner_decision,
          $numeric_pos,
          ${text_nonempty.opt}
        )
        ON CONFLICT (c_program_id, c_ta_category) DO UPDATE SET
          c_decision = EXCLUDED.c_decision,
          c_ranking  = EXCLUDED.c_ranking,
          c_comments = CASE WHEN $bool THEN EXCLUDED.c_comments ELSE t.c_comments END
      """.command

    // Removes the allocations carrying a recommended time for the category in
    // every band not in the list.
    val DeleteOtherRecommendedAllocations: Command[(Program.Id, TimeAccountingCategory, List[ScienceBand])] =
      sql"""
        DELETE FROM t_allocation
         WHERE c_program_id  = $program_id
           AND c_ta_category = $time_accounting_category
           AND c_recommended_time IS NOT NULL
           AND NOT (c_science_band = ANY($_science_band))
      """.command

    // Sets the recommended times, and the allocated durations to match,
    // leaving rows that already have both values alone.
    def upsertRecommendedAllocations(rows: Int): Command[List[(Program.Id, TimeAccountingCategory, ScienceBand, TimeSpan, TimeSpan)]] =
      sql"""
        INSERT INTO t_allocation AS t (
          c_program_id,
          c_ta_category,
          c_science_band,
          c_duration,
          c_recommended_time
        ) VALUES ${(program_id *: time_accounting_category *: science_band *: time_span *: time_span).values.list(rows)}
        ON CONFLICT (c_program_id, c_ta_category, c_science_band) DO UPDATE SET
          c_duration         = EXCLUDED.c_duration,
          c_recommended_time = EXCLUDED.c_recommended_time
        WHERE (t.c_duration, t.c_recommended_time) IS DISTINCT FROM (EXCLUDED.c_duration, EXCLUDED.c_recommended_time)
      """.command
