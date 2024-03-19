// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Semigroup
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.ProposalClassInput
import lucuma.odb.graphql.input.ProposalInput
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.syntax.all.*

import Services.Syntax.*

private[service] trait ProposalService[F[_]] {

  /**
   * Insert the proposal associated with the specified program.
   */
  def insertProposal(SET: ProposalInput.Create, pid: Program.Id)(using Transaction[F]): F[Unit]

  /**
   * Update the proposals associated with the programs in temporary table `t_program_update`
   * (created by ProgramService#updatePrograms). If the update specifies enough information to
   * replace the proposal class then it will be replaced in its entirety (potentially
   * changing the proposal class). Otherwise the proposal class will be updated in-place, but only
   * where the class matches what `SET` specifies. This action also assumes the
   * presence of the `t_program_update` table.
   */
  def updateProposals(SET: ProposalInput.Edit)(using Transaction[F]): F[Result[List[Program.Id]]]

}

object ProposalService {

  sealed trait UpdateProposalsError extends Product with Serializable {
    def user: User
    import UpdateProposalsError.*
    def message: String = this match
      case CreationFailed(_) => 
        "One or more programs has no proposal, and there is insufficient information to create one. To add a proposal all required fields must be specified."
      case InconsistentUpdate(_) =>
        "The specified edits for proposal class do not match the proposal class for one or more specified programs' proposals. To change the proposal class you must specify all fields for that class."

    def failure = odbError.asFailure

    def odbError: OdbError =
      OdbError.InvalidArgument(Some(message))

  }
  
  object UpdateProposalsError {
    case class CreationFailed(user: User)     extends UpdateProposalsError
    case class InconsistentUpdate(user: User) extends UpdateProposalsError
  }

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProposalService[F] =
    new ProposalService[F] {

      def insertProposal(SET: ProposalInput.Create, pid: Program.Id)(using Transaction[F]): F[Unit] =
        session.prepareR(Statements.InsertProposal).use(_.execute(pid, SET)) >>
        partnerSplitsService.insertSplits(SET.partnerSplits, pid)

      def updateProposals(SET: ProposalInput.Edit)(using Transaction[F]): F[Result[List[Program.Id]]] = {

        // Update existing proposals. This will fail if SET.asCreate.isEmpty and there is a class mismatch.
        val update: F[Result[List[Program.Id]]] =
          Statements.updateProposals(SET).fold(Result(Nil).pure[F]) { af =>
            session.prepareR(af.fragment.query(program_id)).use { ps =>
              ps.stream(af.argument, 1024)
                .compile
                .toList
                .map(Result.apply)
            }
            .recover {
              case SqlState.NotNullViolation(e) if e.columnName == Some("c_class") =>
                UpdateProposalsError.InconsistentUpdate(user).failure
            }
          }

        // Insert new proposals. This will fail if there's not enough information.
        val insert: F[Result[List[Program.Id]]] =
          session.prepareR(Statements.InsertProposals).use { ps =>
            ps.stream(SET, 1024)
              .compile
              .toList
              .map(Result.apply)
          }
          .recover {
            case SqlState.NotNullViolation(ex) =>
              UpdateProposalsError.CreationFailed(user).failure
          }

        // Replace the splits
        def replaceSplits: F[List[Program.Id]] =
          SET.partnerSplits.fold(Nil.pure[F]) { ps =>
            partnerSplitsService.updateSplits(ps)
          }

        // Done!
        (for {
          u <- ResultT(update)
          i <- ResultT(insert)
          s <- ResultT(replaceSplits.map(Result.apply))
        } yield (u |+| i |+| s)).value

      }

    }

  private object Statements {

    enum ProposalClassUpdate {
      case None
      case Partial(tag: Tag)
      case Total(tag: Tag)
    }

    def updates(SET: ProposalInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

      // Top-level properties
      val nonProposalClassUpdates: List[AppliedFragment] =
        List(
          SET.abstrakt.foldPresent(sql"c_abstract = ${text_nonempty.opt}"),
          SET.category.foldPresent(sql"c_category = ${tag.opt}"),
          SET.title.foldPresent(sql"c_title = ${text_nonempty.opt}"),
          SET.toOActivation.map(sql"c_too_activation = $too_activation"),
        ).flatten

      // Properties associated with the proposal class (other than the class tag itself)
      val proposalClassUpdates: List[AppliedFragment] =
        SET.proposalClass.toList.flatMap {
          case Left(ta) =>
            List(
              ta.minPercentTime.map(sql"c_min_percent = $int_percent"),
              void"c_min_percent_total = null".some,
              void"c_total_time = null".some,
            ).flatten
          case Right(tb) =>
            List(
              tb.minPercentTime.map(sql"c_min_percent = $int_percent"),
              tb.minPercentTotalTime.map(sql"c_min_percent_total = $int_percent"),
              tb.totalTime.map(sql"c_total_time = $time_span"),
            ).flatten
        }

      // The class tag itself is tricky. If it's a total replacementr then we just update it like
      // normal. But if it's partial we have to be sure it matches what's already there. We do this
      // by updating it to null on a mismatch, which will cause a constraint violation we can
      // catch. Gross, right?
      val proposalClassTag: Option[AppliedFragment] =
        SET.proposalClass.updateType match {
          case ProposalClassUpdate.None  => none
          case ProposalClassUpdate.Total(t) => sql"c_class = $tag".apply(t).some
          case ProposalClassUpdate.Partial(t) => sql"c_class = case when c_class = $tag then c_class end".apply(t).some // force constraint violation on mismatch
        }

      NonEmptyList.fromList(nonProposalClassUpdates ++ proposalClassUpdates ++ proposalClassTag.toList)

    }

    extension (e: Option[Either[ProposalClassInput.TypeA.Edit, ProposalClassInput.TypeB.Edit]]) def updateType: ProposalClassUpdate =
      e match {
        case None => ProposalClassUpdate.None
        case Some(e) =>
          e match {
            case Left(ta)  => if ta.asCreate.isDefined then ProposalClassUpdate.Total(ta.tag) else ProposalClassUpdate.Partial(ta.tag)
            case Right(tb) => if tb.asCreate.isDefined then ProposalClassUpdate.Total(tb.tag) else ProposalClassUpdate.Partial(tb.tag)
          }
      }

    def updateProposals(SET: ProposalInput.Edit): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"""
          UPDATE t_proposal
          SET """ |+| us.intercalate(void", ") |+| void"""
          FROM t_program_update
          WHERE t_proposal.c_program_id = t_program_update.c_program_id
          AND t_program_update.c_has_proposal = true
          RETURNING t_proposal.c_program_id
        """
      }

    /** Insert a proposal. */
    val InsertProposal: Command[(Program.Id, ProposalInput.Create)] =
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_title,
          c_abstract,
          c_category,
          c_too_activation,
          c_class,
          c_min_percent,
          c_min_percent_total,
          c_total_time
        ) VALUES (
          ${program_id},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          ${tag.opt},
          ${too_activation},
          ${tag},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt}
        )
      """.command
         .contramap {
            case (pid, ppi) => (
              pid,
              ppi.title.toOption,
              ppi.abstrakt.toOption,
              ppi.category.toOption,
              ppi.toOActivation,
              ppi.proposalClass.fold(_.tag, _.tag),
              ppi.proposalClass.fold(_.minPercentTime, _.minPercentTime),
              ppi.proposalClass.toOption.map(_.minPercentTotalTime),
              ppi.proposalClass.toOption.map(_.totalTime)
            )
         }

    /** Insert proposals into all programs lacking one, based on the t_program_update temporary table. */
    val InsertProposals: Query[ProposalInput.Edit, Program.Id] =
      sql"""
        INSERT INTO t_proposal (
          c_program_id,
          c_title,
          c_abstract,
          c_category,
          c_too_activation,
          c_class,
          c_min_percent,
          c_min_percent_total,
          c_total_time
        )
        SELECT
          c_program_id,
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          ${tag.opt},
          ${too_activation.opt},
          ${tag.opt},
          ${int_percent.opt},
          ${int_percent.opt},
          ${time_span.opt}
        FROM t_program_update
        WHERE c_has_proposal = false
        RETURNING c_program_id
      """.query(program_id)
         .contramap {
            case ppi => (
              ppi.title.toOption,
              ppi.abstrakt.toOption,
              ppi.category.toOption,
              ppi.toOActivation,
              ppi.proposalClass.map(_.fold(_.tag, _.tag)),
              ppi.proposalClass.flatMap(_.fold(_.minPercentTime, _.minPercentTime)),
              ppi.proposalClass.flatMap(_.toOption.flatMap(_.minPercentTotalTime)),
              ppi.proposalClass.flatMap(_.toOption.flatMap(_.totalTime))
            )
         }

    def insertPartnerSplits(splits: Map[Tag, IntPercent]): Command[(Program.Id, splits.type)] =
      sql"""
         INSERT INTO t_partner_split (c_program_id, c_partner, c_percent)
         VALUES ${(program_id *: tag *: int_percent).values.list(splits.size)}
      """.command
         .contramap {
          case (pid, splits) => splits.toList.map { case (t, p) => (pid, t, p) }
         }

    /** Query program_id ~ bool, where the boolean indicates the presence of a proposal. */
    def programsWithProposals(which: AppliedFragment): AppliedFragment =
      void"""
        SELECT which.pid, p.c_program_id IS NOT NULL
        FROM (""" |+| which |+| void""") as which (pid)
        LEFT JOIN t_proposal p
        ON p.c_program_id = which.pid
      """

  }

}
