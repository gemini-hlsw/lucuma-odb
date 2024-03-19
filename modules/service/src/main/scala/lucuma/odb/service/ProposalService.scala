// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Access
import lucuma.core.model.IntPercent
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.odb.data.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateProposalInput
import lucuma.odb.graphql.input.ProposalClassInput
import lucuma.odb.graphql.input.ProposalPropertiesInput
import lucuma.odb.graphql.input.SetProposalStatusInput
import lucuma.odb.graphql.input.UpdateProposalInput
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
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

  /**
   * Set the proposal status associated with the program specified in the `input`.
   */
  def setProposalStatus(input: SetProposalStatusInput)(using Transaction[F]): F[Result[Program.Id]]
}

object ProposalService {

  sealed trait UpdateProposalError extends Product with Serializable {
    import UpdateProposalError.*
    def message: String = this match
      case CreationFailed(pid) => 
        s"Proposal creation failed because program $pid already has a proposal."
      case UpdateFailed(pid) =>
        s"Proposal update failed because program $pid does not have a proposal."
      case InconsistentUpdate(pid) =>
        s"The specified edits for proposal class do not match the proposal class for program $pid. To change the proposal class you must specify all fields for that class."
      case InvalidProposalStatus(ps) =>
        s"Invalid proposal status: ${ps.value}"
      case NotAuthorizedNewProposalStatus(pid, user, ps) =>
        s"User ${user.id} not authorized to set proposal status to ${ps.value.toUpperCase} in program $pid."
      case NotAuthorizedOldProposalStatus(pid, user, ps) =>
        s"User ${user.id} not authorized to change proposal status from ${ps.value.toUpperCase} in program $pid."
      case InvalidProgramType(pid, progType) =>
        s"Program $pid is of type $progType. Only Science programs can have proposals."
      case NoProposalForStatusChange(pid) =>
        s"Proposal status in program $pid cannot be changed because it has no proposal."
      case NoSemesterForSubmittedProposal(pid) =>
        s"Submitted program $pid must be associated with a semester."
      case NoScienceSubtypeForSubmittedProposal(pid) =>
        s"Submitted program $pid must have a science subtype."

    def failure = odbError.asFailure

    def problem = odbError.asProblem

    def odbError: OdbError = this match
      case CreationFailed(_)                             => OdbError.InvalidArgument(Some(message))
      case UpdateFailed(_)                               => OdbError.InvalidArgument(Some(message))
      case InconsistentUpdate(_)                         => OdbError.InvalidArgument(Some(message))
      case InvalidProposalStatus(_)                      => OdbError.InvalidArgument(Some(message))
      case NotAuthorizedNewProposalStatus(pid, user, ps) => OdbError.NotAuthorized(user.id, Some(message))
      case NotAuthorizedOldProposalStatus(pid, user, ps) => OdbError.NotAuthorized(user.id, Some(message))
      case InvalidProgramType(pid, progType)             => OdbError.InvalidProgram(pid, Some(message))
      case NoProposalForStatusChange(pid)                => OdbError.InvalidProgram(pid, Some(message))
      case NoSemesterForSubmittedProposal(pid)           => OdbError.InvalidProgram(pid, Some(message))
      case NoScienceSubtypeForSubmittedProposal(pid)     => OdbError.InvalidProgram(pid, Some(message))

  }
  
  object UpdateProposalError {
    case class CreationFailed(pid: Program.Id) extends UpdateProposalError
    case class UpdateFailed(pid: Program.Id) extends UpdateProposalError
    case class InconsistentUpdate(pid: Program.Id) extends UpdateProposalError
    // we should never get this one, but we are converting between a Tag and a dynamic enum...
    case class InvalidProposalStatus(ps: Tag) extends UpdateProposalError
    case class NotAuthorizedNewProposalStatus(pid: Program.Id, user: User, ps: Tag) extends UpdateProposalError
    case class NotAuthorizedOldProposalStatus(pid: Program.Id, user: User, ps: Tag) extends UpdateProposalError
    case class InvalidProgramType(pid: Program.Id, progType: ProgramType) extends UpdateProposalError
    case class NoProposalForStatusChange(pid: Program.Id) extends UpdateProposalError
    case class NoSemesterForSubmittedProposal(pid: Program.Id) extends UpdateProposalError
    // we shouldn't be able to get this either, but I saw it once in development.
    case class NoScienceSubtypeForSubmittedProposal(pid: Program.Id) extends UpdateProposalError
  }

  /** Construct a `ProposalService` using the specified `Session`. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProposalService[F] =
    new ProposalService[F] {

      // validates the program type, and also checks to see if the user has access
      private def validateProgramType(pid: Program.Id): F[Result[Unit]] = 
        val af = Statements.getProgramType(user, pid)
        session.prepareR(af.fragment.query(program_type)).use { ps =>
          ps.option(af.argument)
            .map {
              case None                                   => OdbError.InvalidProgram(pid).asFailure
              case Some(ps) if ps === ProgramType.Science => Result.unit
              case Some(ps)                               => UpdateProposalError.InvalidProgramType(pid, ps).failure
            }
        }


      def createProposal(input: CreateProposalInput)(using Transaction[F]): F[Result[Program.Id]] =
        def insert(pid: Program.Id): F[Result[Program.Id]] =
          val af = Statements.insertProposal(user, pid, input.SET)
          session.prepareR(af.fragment.query(program_id)).use { ps =>
            ps.option(af.argument)
              .map(Result.fromOption(_, OdbError.InvalidProgram(pid).asProblem  ))
              .recover {
               case SqlState.UniqueViolation(e) => UpdateProposalError.CreationFailed(pid).failure
              }
          }

        (for {
          _   <- ResultT(validateProgramType(input.programId))
          _   <- ResultT(insert(input.programId))
          _   <- ResultT(partnerSplitsService.insertSplits(input.SET.partnerSplits, input.programId).map(Result.success))
        } yield input.programId).value

      def updateProposal(input: UpdateProposalInput)(using Transaction[F]): F[Result[Program.Id]] = {
        def update(pid: Program.Id): F[Result[Program.Id]] =
          Statements.updateProposal(pid, input.SET).fold(Result(pid).pure[F]) { af =>
            session.prepareR(af.fragment.query(program_id)).use { ps =>
              ps.option(af.argument).map(Result.fromOption(_, UpdateProposalError.UpdateFailed(pid).problem))
            }
            .recover {
              case SqlState.NotNullViolation(e) if e.columnName == Some("c_class") =>
                UpdateProposalError.InconsistentUpdate(pid).failure
            }
          }
        
        def replaceSplits(pid: Program.Id): F[Result[Unit]] =
          input.SET.partnerSplits.fold(().pure[F]) { ps =>
            partnerSplitsService.updateSplits(ps, pid)
          }.map(Result.success)

        (for {
          pid <- ResultT(
                   programService.resolvePid(input.programId, input.proposalReference, input.programReference)
                 )
          _   <- ResultT(validateProgramType(pid))
          _   <- ResultT(update(pid))
          _   <- ResultT(replaceSplits(pid))
        } yield pid).value
      }

      def setProposalStatus(input: SetProposalStatusInput)(using Transaction[F]): F[Result[Program.Id]] = {
        // A stable identifier (ie. a `val`) is needed for the enums.
        val enumsVal = enums

        def tagToProposalStatus(tag: Tag): Result[enumsVal.ProposalStatus] =
          Enumerated[enumsVal.ProposalStatus]
            .fromTag(tag.value)
            .fold(UpdateProposalError.InvalidProposalStatus(tag).failure)(Result.apply)

        def userCanChangeProposalStatus(ps: enumsVal.ProposalStatus): Boolean =
          user.role.access =!= Access.Guest && (ps <= enumsVal.ProposalStatus.Submitted || user.role.access >= Access.Ngo)

        case class ProposalInfo(
          statusTag: Tag,
          hasProposal: Boolean,
          programType: ProgramType,
          semester: Option[Semester],
          scienceSubtype: Option[ScienceSubtype]
        ):
          val status: Result[enumsVal.ProposalStatus] = tagToProposalStatus(statusTag)

        def getInfo(pid: Program.Id): F[Result[ProposalInfo]] =
          val af = Statements.getCurrentProposalStatusInfo(user, pid)
          session.prepareR(
            af.fragment.query(tag *: bool *: program_type *: semester.opt *: science_subtype.opt).to[ProposalInfo]
            ).use { ps =>
              ps.option(af.argument)
                .map(Result.fromOption(_, OdbError.InvalidProgram(pid).asProblem))
            }
        
        def validateProgramReference(pid: Program.Id, info: ProposalInfo, newStatus: enumsVal.ProposalStatus): Result[Unit] =
          (
            UpdateProposalError.NoSemesterForSubmittedProposal(pid)
              .failure.unlessA(newStatus === enumsVal.ProposalStatus.NotSubmitted || info.semester.isDefined),
            UpdateProposalError.NoScienceSubtypeForSubmittedProposal(pid)
              .failure.unlessA(newStatus === enumsVal.ProposalStatus.NotSubmitted || info.scienceSubtype.isDefined),

          ).tupled.void

        def validate(
          pid: Program.Id,
          info: ProposalInfo,
          oldStatus: enumsVal.ProposalStatus,
          newStatus: enumsVal.ProposalStatus
        ): Result[Unit] =
          for {
            _ <- UpdateProposalError.InvalidProgramType(pid, info.programType)
                  .failure.unlessA(info.programType === ProgramType.Science)
            _ <- UpdateProposalError.NoProposalForStatusChange(pid)
                  .failure.unlessA(info.hasProposal)
            _ <- UpdateProposalError.NotAuthorizedNewProposalStatus(pid, user, Tag(newStatus.tag))
                  .failure.unlessA(userCanChangeProposalStatus(newStatus))
            _ <- UpdateProposalError.NotAuthorizedOldProposalStatus(pid, user, info.statusTag)
                  .failure.unlessA(userCanChangeProposalStatus(oldStatus))
            _ <- validateProgramReference(pid, info, newStatus)
          } yield ()

        def update(pid: Program.Id, tag: Tag): F[Unit] = 
          val af = Statements.updateProposalStatus(user, pid, tag)
          session.prepareR(af.fragment.command).use(_.execute(af.argument)).void

        (for {
          pid       <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
          info      <- ResultT(getInfo(pid))
          oldStatus <- ResultT(info.status.pure) // This 'should' be succesful, since it is from the DB
          newStatus <- ResultT(tagToProposalStatus(input.status).pure)
          _         <- ResultT(validate(pid, info, oldStatus, newStatus).pure)
          _         <- ResultT(update(pid, input.status).map(Result.pure))
        } yield pid).value
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

    def updateProposal(pid: Program.Id, SET: ProposalPropertiesInput.Edit): Option[AppliedFragment] =
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

    def getProgramType(user: User, pid: Program.Id): AppliedFragment =
      sql"""
        SELECT c_program_type
        FROM t_program
        WHERE c_program_id = $program_id
      """.apply(pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

    def getCurrentProposalStatusInfo(user: User, pid: Program.Id): AppliedFragment =
      sql"""
        SELECT 
          prog.c_proposal_status,
          prop.c_program_id IS NOT NULL,
          prog.c_program_type,
          prog.c_semester,
          prog.c_science_subtype
        FROM t_program prog
        LEFT JOIN t_proposal prop
          ON prog.c_program_id = prop.c_program_id
        WHERE prog.c_program_id = $program_id
      """.apply(pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)

    def updateProposalStatus(user: User, pid: Program.Id, status: Tag): AppliedFragment =
      sql"""
        UPDATE t_program
        SET c_proposal_status = $tag
        WHERE c_program_id = $program_id
      """.apply(status, pid) |+|
      ProgramService.Statements.andWhereUserAccess(user, pid)
  }

}
