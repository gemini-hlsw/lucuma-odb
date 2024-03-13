// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

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
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.ProposalClassInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.syntax.all.*

import Services.Syntax.*

private[service] trait ProposalService[F[_]] {

  /**
   * Create a proposal associated with the program specified in `input`.
   */
  def createProposal(input: CreateProposalInput)(using Transaction[F]): F[Result[Program.Id]]

  /**
   * Update a proposal associated with the program specified in the `input`.
   */
  def updateProposal(input: UpdateProposalInput)(using Transaction[F]): F[Result[Program.Id]]
}

object ProposalService {

  sealed trait UpdateProposalsError extends Product with Serializable {
    import UpdateProposalsError.*
    def message: String = this match
      case CreationFailed(pid)     => 
        s"Proposal creation failed because program $pid already has a proposal."
      case UpdateFailed(pid)       =>
        s"Proposal update failed because program $pid does not have a proposal."
      case InconsistentUpdate(pid) =>
        s"The specified edits for proposal class do not match the proposal class for program $pid. To change the proposal class you must specify all fields for that class."

    def failure = odbError.asFailure

    def problem = odbError.asProblem

    def odbError: OdbError = OdbError.InvalidArgument(Some(message))

  }
  
  object UpdateProposalsError {
    case class CreationFailed(pid: Program.Id)     extends UpdateProposalsError
    case class UpdateFailed(pid: Program.Id)       extends UpdateProposalsError
    case class InconsistentUpdate(pid: Program.Id) extends UpdateProposalsError
  }

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProposalService[F] =
    new ProposalService[F] {

      def createProposal(input: CreateProposalInput)(using Transaction[F]): F[Result[Program.Id]] =
        def insert(pid: Program.Id): F[Result[Program.Id]] =
          val af = Statements.insertProposal(user, pid, input.SET)
          session.prepareR(af.fragment.query(program_id)).use { ps =>
            ps.option(af.argument)
              .map(Result.fromOption(_, OdbError.InvalidProgram(pid).asProblem  ))
              .recover {
               case SqlState.UniqueViolation(e) => UpdateProposalsError.CreationFailed(pid).failure
              }
          }

        (for {
          p  <- ResultT(
                  programService.resolvePid(input.programId, input.proposalReference, input.programReference)
                )
          _  <- ResultT(insert(p))
          _  <- ResultT(partnerSplitsService.insertSplits(input.SET.partnerSplits, p).map(Result.success))
        } yield p).value

      def updateProposal(input: UpdateProposalInput)(using Transaction[F]): F[Result[Program.Id]] = {
        def update(pid: Program.Id): F[Result[Program.Id]] =
          Statements.updateProposal(input.SET, pid).fold(Result(pid).pure[F]) { af =>
            session.prepareR(af.fragment.query(program_id)).use { ps =>
              ps.option(af.argument).map(Result.fromOption(_, UpdateProposalsError.UpdateFailed(pid).problem))
            }
            .recover {
              case SqlState.NotNullViolation(e) if e.columnName == Some("c_class") =>
                UpdateProposalsError.InconsistentUpdate(pid).failure
            }
          }
        
        def replaceSplits(pid: Program.Id): F[Result[Unit]] =
          input.SET.partnerSplits.fold(().pure[F]) { ps =>
            partnerSplitsService.updateSplits(ps, pid)
          }.map(Result.success)

        def hasAccess(pid: Program.Id): F[Result[Unit]] =
          programService.userHasAccess(pid).map(b =>
            if (b) Result.unit
            else OdbError.InvalidProgram(pid).asFailure
          )

        (for {
          p  <- ResultT(
                  programService.resolvePid(input.programId, input.proposalReference, input.programReference)
                )
          _  <- ResultT(hasAccess(p))
          _  <- ResultT(update(p))
          _  <- ResultT(replaceSplits(p))
        } yield p).value
      }
    }

  private object Statements {

    enum ProposalClassUpdate {
      case None
      case Partial(tag: Tag)
      case Total(tag: Tag)
    }

    def updates(SET: ProposalPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

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

      // The class tag itself is tricky. If it's a total replacement then we just update it like
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

    def updateProposal(SET: ProposalPropertiesInput.Edit, pid: Program.Id): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"""
          UPDATE t_proposal
          SET """ |+| us.intercalate(void", ") |+| 
        sql"""
          WHERE t_proposal.c_program_id = $program_id
          RETURNING t_proposal.c_program_id
        """.apply(pid)
      }

    /** Insert a proposal. */
    def insertProposal(user: User, pid: Program.Id, ppi: ProposalPropertiesInput.Create): AppliedFragment =
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
        ) SELECT
          ${program_id},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          ${tag.opt},
          ${too_activation},
          ${tag},
          ${int_percent},
          ${int_percent.opt},
          ${time_span.opt}
      """.apply(
              pid,
              ppi.title.toOption,
              ppi.abstrakt.toOption,
              ppi.category.toOption,
              ppi.toOActivation,
              ppi.proposalClass.fold(_.tag, _.tag),
              ppi.proposalClass.fold(_.minPercentTime, _.minPercentTime),
              ppi.proposalClass.toOption.map(_.minPercentTotalTime),
              ppi.proposalClass.toOption.map(_.totalTime)
            ) |+|
      ProgramService.Statements.whereUserAccess(user, pid) |+| 
      void"""
         RETURNING t_proposal.c_program_id
      """
      
  }

}
