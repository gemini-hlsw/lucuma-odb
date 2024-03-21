// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.Semigroup
import cats.data.Ior
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.ServiceRole
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.odb.data.*
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.input.ProgramReferencePropertiesInput
import lucuma.odb.graphql.input.SetProgramReferenceInput
import lucuma.odb.service.ProgramService.LinkUserRequest.PartnerSupport
import lucuma.odb.service.ProgramService.LinkUserRequest.StaffSupport
import lucuma.odb.service.ProgramService.LinkUserResponse.Success
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import OdbErrorExtensions.*
import Services.Syntax.*

trait ProgramService[F[_]] {

  /**
   * Find the program id matching the given reference, if any.
   */
  def selectPid(ref: Ior[ProposalReference, ProgramReference]): F[Option[Program.Id]]

  /**
   * Convenience method. Find the program id consistent with the provided ids (if any).
   */
  def resolvePid(
    pid:  Option[Program.Id],
    prop: Option[ProposalReference],
    prog: Option[ProgramReference]
  ): F[Result[Program.Id]]

  def setProgramReference(input: SetProgramReferenceInput): F[Result[(Program.Id, Option[ProgramReference])]]

  /**
   * Insert a new program, where the calling user becomes PI (unless it's a Service user, in which
   * case the PI is left empty.
   */
  def insertProgram(SET: Option[ProgramPropertiesInput.Create])(using Transaction[F]): F[Program.Id]

  /**
   * Perform the requested program <-> user link, yielding the linked ids if successful, or None
   * if the user was not authorized to perform the action.
   */
  def linkUser(req: ProgramService.LinkUserRequest)(using Transaction[F]): F[Result[(Program.Id, User.Id)]]

  /** Update the properies for programs with ids given by the supplied fragment, yielding a list of affected ids. */
  def updatePrograms(SET: ProgramPropertiesInput.Edit, where: AppliedFragment)(using Transaction[F]): F[Result[List[Program.Id]]]

  /** Check to see if the user has access to the given program. */
  def userHasAccess(programId: Program.Id)(using Transaction[F]): F[Boolean]

}

object ProgramService {

  sealed abstract class LinkUserRequest(val role: ProgramUserRole, val supportType: Option[ProgramUserSupportType] = None, val supportPartner: Option[Tag] = None) {
    def programId: Program.Id
    def userId: User.Id
  }
  object LinkUserRequest {

    case class Coi(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Coi)
    case class Observer(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Observer)
    case class StaffSupport(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Support, Some(ProgramUserSupportType.Staff))
    case class PartnerSupport(programId: Program.Id, userId: User.Id, partnerTag: Tag) extends LinkUserRequest(ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(partnerTag))

    /** Construct a LinkedUserRequest from unvalidated inputs, if possible. */
    def validate(
      programId: Program.Id,
      userId: User.Id,
      role: ProgramUserRole,
      supportType: Option[ProgramUserSupportType],
      supportPartner: Option[Tag]
    ): Either[String, LinkUserRequest] =
      role match {
        case ProgramUserRole.Coi =>
          (supportType orElse supportPartner)
            .as("Support type/partner must not be specified for COI role.")
            .toLeft(Coi(programId, userId))
        case ProgramUserRole.Observer =>
          (supportType orElse supportPartner)
            .as("Support type/partner must not be specified for OBSERVER role.")
            .toLeft(Observer(programId, userId))
        case ProgramUserRole.Support =>
          (supportType, supportPartner) match {
            case (Some(ProgramUserSupportType.Staff), None)        => Right(StaffSupport(programId, userId))
            case (Some(ProgramUserSupportType.Staff), _)           => Left("Support partner must not be specified if support type is STAFF.")
            case (Some(ProgramUserSupportType.Partner), Some(tag)) => Right(PartnerSupport(programId, userId, tag))
            case (Some(ProgramUserSupportType.Partner), _)         => Left("Support partner must be specified if support type is PARTNER.")
            case (None, _)                                         => Left("Support type must be specifed if role is SUPPORT.")
          }
      }

  }

  sealed trait LinkUserResponse extends Product with Serializable
  object LinkUserResponse {
    case class NotAuthorized(user: User)                     extends LinkUserResponse
    case class AlreadyLinked(pid: Program.Id, user: User.Id) extends LinkUserResponse
    case class Success(pid: Program.Id, user: User.Id)       extends LinkUserResponse
    case class InvalidUser(user: User.Id)                    extends LinkUserResponse
  }

  sealed trait UpdateProgramsError extends Product with Serializable {
    import UpdateProgramsError.*
    def message: String = this match
      case InvalidProposalStatus(_, ps)                     =>
        s"Invalid proposal status: ${ps.value}"
      case NotAuthorizedNewProposalStatus(user, ps)      =>
        s"User ${user.id} not authorized to set proposal status to ${ps.value.toUpperCase}."
      case NotAuthorizedOldProposalStatus(pid, user, ps) =>
        s"User ${user.id} not authorized to change proposal status from ${ps.value.toUpperCase} in program $pid."
      case NoProposalForStatusChange(pid)                =>
        s"Proposal status in program $pid cannot be changed because it has no proposal."
      case NoSemesterForSubmittedProposal(pid)           =>
        s"Submitted program $pid must be associated with a semester."
      case InvalidSemester(s)                            =>
        s"The maximum semester is capped at the current year +1${s.fold(".")(" ("+ _.format + " specified).")}"
      case DuplicateReference(r)                         =>
        s"""Program reference${r.fold("")(s => s" '$s'")} already exists."""

    def failure = this match
      case InvalidProposalStatus(user, ps) => OdbError.InvalidArgument(Some(message)).asFailure
      case NotAuthorizedNewProposalStatus(user, ps) => OdbError.NotAuthorized(user.id, Some(message)).asFailure
      case NotAuthorizedOldProposalStatus(pid, user, ps) => OdbError.NotAuthorized(user.id, Some(message)).asFailure
      case NoProposalForStatusChange(pid) => OdbError.InvalidProgram(pid, Some(message)).asFailure
      case NoSemesterForSubmittedProposal(pid) => OdbError.InvalidProgram(pid, Some(message)).asFailure
      case InvalidSemester(s) => OdbError.InvalidArgument(Some(message)).asFailure
      case DuplicateReference(r) => OdbError.InvalidArgument(Some(message)).asFailure

  }

  object UpdateProgramsError {
    // we should never get this one, but we are converting between a Tag and a dynamic enum...
    case class InvalidProposalStatus(user: User, ps: Tag) extends UpdateProgramsError
    case class NotAuthorizedNewProposalStatus(user: User, ps: Tag) extends UpdateProgramsError
    case class NotAuthorizedOldProposalStatus(pid: Program.Id, user: User, ps: Tag) extends UpdateProgramsError
    case class NoProposalForStatusChange(pid: Program.Id) extends UpdateProgramsError
    case class NoSemesterForSubmittedProposal(pid: Program.Id) extends UpdateProgramsError
    case class InvalidSemester(s: Option[Semester]) extends UpdateProgramsError
    case class DuplicateReference(ref: Option[String]) extends UpdateProgramsError
  }

  /**
   * Construct a `ProgramService` using the specified `Session`, for the specified `User`. All
   * operations will be performed on behalf of `user`.
   */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProgramService[F] =
    new ProgramService[F] {

      def selectPid(ref: Ior[ProposalReference, ProgramReference]): F[Option[Program.Id]] = {
        val af = Statements.selectPid(ref)
        session.prepareR(af.fragment.query(program_id)).use { ps =>
          ps.option(af.argument)
        }
      }

      override def resolvePid(
        pid:  Option[Program.Id],
        prop: Option[ProposalReference],
        prog: Option[ProgramReference]
      ): F[Result[Program.Id]] = {
        def notFound(ref: Ior[ProposalReference, ProgramReference]): String =
          ref.fold(
            r => s"Proposal '${r.label}' was not found.",
            r => s"Program '${r.label}' was not found.",
            (r0, r1) => s"Proposal '${r0.label}' and program '${r1.label}' were not found or do not correspond to the same program."
          )

        def notCorresponding(
          ref:      Ior[ProposalReference, ProgramReference],
          pid:      Program.Id,
          givenPid: Program.Id
        ): String =
          ref.fold(
            r => s"Proposal '${r.label}' (id $pid) does not correspond to the specified program id $givenPid.",
            r => s"Program '${r.label}' (id $pid) does not correspond to the specified program id $givenPid.",
            (r0, r1) => s"Proposal '${r0.label}' and program '${r1.label}' (id $pid) do not correspond to the specified program id $givenPid."
          )

        (pid, Ior.fromOptions(prop, prog)) match {
          case (None, None)    => OdbError.InvalidArgument("One of programId, programReference or proposalReference must be provided.".some).asFailureF
          case (Some(p), None) => p.success.pure[F]
          case (_, Some(r))    => selectPid(r).map { op =>
            op.fold(OdbError.InvalidArgument(notFound(r).some).asFailure) { selectedPid =>
              pid.fold(selectedPid.success) { givenPid =>
                OdbError.InvalidArgument(notCorresponding(r, selectedPid, givenPid).some)
                  .asFailure
                  .unlessA(selectedPid === givenPid)
                  .as(selectedPid)
              }
            }
          }
        }
      }

      private def setProgramReferenceImpl(id: Program.Id, input: ProgramReferencePropertiesInput): F[Result[Option[ProgramReference]]] =
        session
          .unique(Statements.SetProgramReference)(id, input)
          .map(_.success)
          .recover {
            case SqlState.CheckViolation(ex) if ex.getMessage.indexOf("d_semester_check") >= 0 =>
              UpdateProgramsError
                .InvalidSemester(input.semester)
                .failure
            case SqlState.UniqueViolation(ex) =>
              // See if we can parse out the duplicate reference string.
              val pat = """\(c_program_reference\)=\(([^)]+)\)""".r
              UpdateProgramsError
                .DuplicateReference(pat.findFirstMatchIn(ex.getMessage).map(_.group(1)))
                .failure
          }

      override def setProgramReference(input: SetProgramReferenceInput): F[Result[(Program.Id, Option[ProgramReference])]] =
        programService.resolvePid(input.programId,input.proposalReference, input.programReference).flatMap: r =>
          r.flatTraverse: pid =>
            setProgramReferenceImpl(pid, input.SET).map: oref =>
              oref.map((pid, _))

      def insertProgram(SET: Option[ProgramPropertiesInput.Create])(using Transaction[F]): F[Program.Id] =
        Trace[F].span("insertProgram") {
          val SETʹ = SET.getOrElse(ProgramPropertiesInput.Create.Empty)

          session.prepareR(Statements.InsertProgram).use(_.unique(SETʹ.name, SETʹ.semester, user)).flatTap { pid =>
            SETʹ.proposal.traverse { proposalInput =>
              proposalService.insertProposal(proposalInput, pid)
            }
          }
        }

      def linkUserImpl(req: ProgramService.LinkUserRequest)(using Transaction[F]): F[LinkUserResponse] = {
        val af: Option[AppliedFragment] =
          req match {
            case LinkUserRequest.Coi(programId, userId) => Statements.linkCoi(programId, userId, user)
            case LinkUserRequest.Observer(programId, userId) => Statements.linkObserver(programId, userId, user)
            case StaffSupport(programId, userId) => Statements.linkStaffSupport(programId, userId, user)
            case PartnerSupport(programId, userId, partnerTag) => Statements.linkPartnerSupport(programId, userId, user, partnerTag)
          }
        af match {
          case None     =>  Monad[F].pure(LinkUserResponse.NotAuthorized(user))
          case Some(af) =>
            val stmt = sql"${af.fragment} RETURNING c_program_id, c_user_id".query(program_id ~ user_id)
            session.prepareR(stmt).use { pq =>
              pq.option(af.argument).map {
                case Some(pid ~ uid) => LinkUserResponse.Success(pid, uid)
                case None            => LinkUserResponse.NotAuthorized(user)
              } .recover {
                case SqlState.UniqueViolation(_) => LinkUserResponse.AlreadyLinked(req.programId, req.userId)
                case SqlState.ForeignKeyViolation(_) => LinkUserResponse.InvalidUser(req.userId)
              }
            }
        }
      }

      def linkUser(req: ProgramService.LinkUserRequest)(using Transaction[F]): F[Result[(Program.Id, User.Id)]] =
        linkUserImpl(req).map:
          case LinkUserResponse.NotAuthorized(user)     => OdbError.NotAuthorized(user.id).asFailure
          case LinkUserResponse.AlreadyLinked(pid, uid) => OdbError.NoAction(Some(s"User $uid is already linked to program $pid.")).asFailure
          case LinkUserResponse.InvalidUser(uid)        => OdbError.InvalidUser(uid, Some(s"User $uid does not exist or is of a nonstandard type.")).asFailure
          case LinkUserResponse.Success(pid, user)      => Result((pid, user))

      def updatePrograms(SET: ProgramPropertiesInput.Edit, where: AppliedFragment)(using Transaction[F]):
        F[Result[List[Program.Id]]] = {

        // Create the temp table with the programs we're updating. We will join with this
        // several times later on in the transaction.
        val setup: F[Unit] = {
          val af = Statements.createProgramUpdateTempTable(where)
          session.prepareR(af.fragment.command).use(_.execute(af.argument)).void
        }

        // Update programs
        val updatePrograms: F[Result[List[Program.Id]]] =
          Statements.updatePrograms(SET).fold(Nil.success.pure[F]) { af =>
            session.prepareR(af.fragment.query(program_id)).use { ps =>
              ps.stream(af.argument, 1024)
                .compile
                .toList
                .map(_.success)
            }
          }.recover {
            case SqlState.CheckViolation(ex) if ex.getMessage.indexOf("d_semester_check") >= 0 =>
              UpdateProgramsError.InvalidSemester(SET.semester.toOption).failure
          }

        // Update proposals. This can fail in a few ways.
        val updateProposals: F[Result[List[Program.Id]]] =
          SET.proposal.fold(Result(Nil).pure[F]) {
            proposalService.updateProposals(_)
          }

        // A stable identifier (ie. a `val`) is needed for the enums.
        val enumsVal = enums

        def tagToProposalStatus(tag: Tag): Result[enumsVal.ProposalStatus] =
          Enumerated[enumsVal.ProposalStatus]
            .fromTag(tag.value)
            .fold(UpdateProgramsError.InvalidProposalStatus(user, tag).failure)(Result.apply)

        def userCanChangeProposalStatus(ps: enumsVal.ProposalStatus): Boolean =
          user.role.access =!= Access.Guest && (ps <= enumsVal.ProposalStatus.Submitted || user.role.access >= Access.Ngo)

        def validateStatusUpdate(
          pid:             Program.Id,
          oldStatus:       enumsVal.ProposalStatus,
          newStatusUpdate: Option[enumsVal.ProposalStatus],
          hasProposal:     Boolean
        ): Result[Unit] =
          newStatusUpdate
            .filter(_ =!= oldStatus) // if they match we're not really trying to update
            .fold(Result.unit) { newStatus =>
              (
                UpdateProgramsError.NoProposalForStatusChange(pid)
                  .failure.unlessA(hasProposal),

                UpdateProgramsError.NotAuthorizedNewProposalStatus(user, Tag(newStatus.tag))
                  .failure.unlessA(userCanChangeProposalStatus(newStatus)),

                UpdateProgramsError.NotAuthorizedOldProposalStatus(pid, user, Tag(oldStatus.tag))
                  .failure.unlessA(userCanChangeProposalStatus(oldStatus))

              ).tupled.void  // do we want all of the errors or would it be annoying?
            }

        def validateUpdate(newStatusUpdate: Option[enumsVal.ProposalStatus]): F[Result[Unit]] =
          session.prepareR(Statements.getTempTableData).use(
            _.stream(Void, chunkSize = 1024)
             .fold(Result.unit) { case (acc, (pid, oldSemester, psTag, hasProposal)) =>
               val check: Result[Unit] =
                 for {
                   oldStatus <- tagToProposalStatus(psTag)
                   _         <- validateStatusUpdate(pid, oldStatus, newStatusUpdate, hasProposal)
                   finalStatus   = newStatusUpdate.getOrElse(oldStatus)
                   finalSemester = SET.semester.fold(none, oldSemester, _.some)
                   _         <- UpdateProgramsError.NoSemesterForSubmittedProposal(pid)
                                  .failure.unlessA(finalStatus === enumsVal.ProposalStatus.NotSubmitted || finalSemester.isDefined)
                 } yield ()

               (acc, check).parTupled.void
             }
             .compile
             .onlyOrError
          )

        (for {
          _    <- ResultT(setup.map(Result.apply))
          n    <- ResultT(SET.proposalStatus.traverse(tagToProposalStatus).pure[F])
          _    <- ResultT(validateUpdate(n))
          ids1 <- ResultT(updatePrograms)
          ids2 <- ResultT(updateProposals) 
        } yield (ids1 |+| ids2)).value

      }

      def userHasAccess(programId: Program.Id)(using Transaction[F]): F[Boolean] =
        Statements.existsUserAccess(user, programId).fold(true.pure[F]) { af =>
          val stmt = sql"SELECT ${af.fragment}".query(bool)
          session.prepareR(stmt).use { pg =>
            pg.unique(af.argument)
          }
        }

    }


  object Statements {

    def selectPid(ref: Ior[ProposalReference, ProgramReference]): AppliedFragment =
      void"""
        SELECT
          c_program_id
        FROM
          t_program
        WHERE
      """ |+| ref.fold(
        sql"c_proposal_reference = $proposal_reference",
        sql"c_program_reference = $program_reference",
        (prop, prog) => sql"c_proposal_reference = $proposal_reference AND c_program_reference = $program_reference".apply(prop, prog)
      )

    val SetProgramReference: Query[(Program.Id, ProgramReferencePropertiesInput), Option[ProgramReference]] =
      sql"""
        UPDATE
          t_program
        SET
          c_program_type    = $program_type,
          c_library_desc    = ${text.opt},
          c_instrument      = ${instrument.opt},
          c_semester        = ${semester.opt},
          c_science_subtype = ${science_subtype.opt}
        WHERE
          c_program_id = $program_id
        RETURNING
          c_program_reference
      """.query(program_reference.opt)
         .contramap[(Program.Id, ProgramReferencePropertiesInput)] { (id, prpi) => (
           prpi.programType,
           prpi.description.map(_.value),
           prpi.instrument,
           prpi.semester,
           prpi.scienceSubtype,
           id
         )}

    def createProgramUpdateTempTable(whichProgramIds: AppliedFragment): AppliedFragment =
      void"""
        CREATE TEMPORARY TABLE t_program_update (
          c_program_id,
          c_semester,
          c_proposal_status,
          c_has_proposal
        )
        ON COMMIT DROP
          AS SELECT
            which.pid,
            prog.c_semester,
            prog.c_proposal_status,
            prop.c_program_id IS NOT NULL
        FROM (""" |+| whichProgramIds |+| void""") AS which (pid)
        INNER JOIN t_program prog
          ON prog.c_program_id = which.pid
        LEFT JOIN t_proposal prop
          ON prop.c_program_id = which.pid
      """

    def getTempTableData: Query[Void, (Program.Id, Option[Semester], Tag, Boolean)] =
      sql"""
        SELECT
           c_program_id,
           c_semester,
           c_proposal_status,
           c_has_proposal
        FROM t_program_update
        ORDER BY c_program_id
      """.query(program_id *: semester.opt *: tag *: bool)

    def updates(SET: ProgramPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.name.map(sql"c_name = $text_nonempty"),
          SET.proposalStatus.map(sql"c_proposal_status = $tag"),
          SET.semester.fold(void"c_semester = null".some, none, s => sql"c_semester = $semester"(s).some),
        ).flatten
      )

    def updatePrograms(SET: ProgramPropertiesInput.Edit): Option[AppliedFragment] =
      updates(SET).map { us =>
        void"""
          UPDATE t_program
          SET """ |+| us.intercalate(void", ") |+| void"""
          FROM t_program_update WHERE t_program.c_program_id = t_program_update.c_program_id
          RETURNING t_program.c_program_id
        """
      }

    def existsUserAsPi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      sql"""
        EXISTS (select c_program_id from t_program where c_program_id = $program_id and c_pi_user_id = $user_id)
      """.apply(programId, userId)

    def existsUserAsCoi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      sql"""
        EXISTS (select c_role from t_program_user where  c_program_id = $program_id and c_user_id = $user_id and c_role = 'coi')
      """.apply(programId, userId)

    def existsAllocationForPartner(
      programId: Program.Id,
      partner: Tag
    ): AppliedFragment =
      sql"""
        EXISTS (select c_duration from t_allocation where c_program_id = $program_id and c_partner=$tag and c_duration > 'PT')
        """.apply(programId, partner)

    def existsUserAccess(
      user:      User,
      programId: Program.Id
    ): Option[AppliedFragment] =
      user.role match {
        case GuestRole             => existsUserAsPi(programId, user.id).some
        case Pi(_)                 => (existsUserAsPi(programId, user.id) |+| void" OR " |+| existsUserAsCoi(programId, user.id)).some
        case Ngo(_, partner)       => existsAllocationForPartner(programId, Tag(partner.tag)).some
        case ServiceRole(_) |
             StandardRole.Admin(_) |
             StandardRole.Staff(_) => none
      }

    def whereUserAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"WHERE " |+| af
      }

    def andWhereUserAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"AND " |+| af
      }

    /** Insert a program, making the passed user PI if it's a non-service user. */
    val InsertProgram: Query[(Option[NonEmptyString], Option[Semester], User), Program.Id] =
      sql"""
        INSERT INTO t_program (c_name, c_semester, c_pi_user_id, c_pi_user_type)
        VALUES (${text_nonempty.opt}, ${semester.opt}, ${(user_id ~ user_type).opt})
        RETURNING c_program_id
      """.query(program_id)
         .contramap {
            case (oNes, oSem, ServiceUser(_, _)) => (oNes, oSem, None)
            case (oNes, oSem, nonServiceUser   ) => (oNes, oSem, Some(nonServiceUser.id, UserType.fromUser(nonServiceUser)))
         }

    /** Link a user to a program, without any access checking. */
    val LinkUser: Fragment[(Program.Id, User.Id, ProgramUserRole, Option[ProgramUserSupportType], Option[Tag])] =
      sql"""
         INSERT INTO t_program_user (c_program_id, c_user_id, c_user_type, c_role, c_support_type, c_support_partner)
         SELECT $program_id, $user_id, 'standard', $program_user_role, ${program_user_support_type.opt}, ${tag.opt}
        """

    /**
     * Link a co-investigator to a program.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI.
     */
    def linkCoi(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram, targetUser, ProgramUserRole.Coi, None, None)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, Tag(partner.tag)))
        case StandardRole.Pi(_)           => Some(up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id))
        case StandardRole.Staff(_)        => Some(up)
      }
    }

    /**
     * Link an observer to a program.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI or Coi.
     */
    def linkObserver(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram, targetUser, ProgramUserRole.Observer, None, None)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, Tag(partner.tag)))
        case StandardRole.Staff(_)        => Some(up)
        case StandardRole.Pi(_)           =>
          Some(
            up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id)  |+|
                   void" OR "    |+| existsUserAsCoi(targetProgram, user.id)
          )
      }
    }

    /**
     * Link staff support to a program.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    def linkStaffSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram, targetUser, ProgramUserRole.Support, Some(ProgramUserSupportType.Staff), None)
      user.role.access match {
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this
      }
    }

    /**
     * Link partner support to a program.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    def linkPartnerSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
      partner: Tag, // partner
    ): Option[AppliedFragment] = {
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram, targetUser, ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(partner))
      user.role.access match {
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this
      }
    }

  }

}
