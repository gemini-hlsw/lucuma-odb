// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.data.Ior
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.model.Access
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
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
import lucuma.odb.data.*
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.input.ProgramReferencePropertiesInput
import lucuma.odb.graphql.input.SetProgramReferenceInput
import lucuma.odb.graphql.input.UnlinkUserInput
import lucuma.odb.service.ProgramService.LinkUserRequest.PartnerSupport
import lucuma.odb.service.ProgramService.LinkUserRequest.StaffSupport
import lucuma.odb.service.ProgramService.LinkUserResponse.Success
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.data.Completion
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

  /** Unlink the requested user, yielding true if the user was unlinkes, false if no such link existed. */
  def unlinkUser(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]]

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
    case class Observer(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.CoiRO)
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
        case ProgramUserRole.CoiRO =>
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
      case InvalidSemester(s)                            =>
        s"The maximum semester is capped at the current year +1${s.fold(".")(" ("+ _.format + " specified).")}"
      case DuplicateReference(r)                         =>
        s"""Program reference${r.fold("")(s => s" '$s'")} already exists."""

    def failure = this match
      case InvalidSemester(s) => OdbError.InvalidArgument(Some(message)).asFailure
      case DuplicateReference(r) => OdbError.InvalidArgument(Some(message)).asFailure

  }

  object UpdateProgramsError {
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

          session.prepareR(Statements.InsertProgram).use(_.unique(SETʹ.name, SETʹ.semester, user))
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

      // delete a link unconditionally
      private def unlinkUnconditionally(input: UnlinkUserInput): F[Result[Boolean]] =
        session.prepareR(Statements.UnlinkUnconditionally).use: pc =>
          pc.execute(input.programId, input.userId).map:
            case Completion.Delete(1) => Result(true)
            case other                => Result.internalError(s"unlinkUnconditionally: unexpected completion: $other")

      private def unlinkCoi(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] =
        user.role.access match
          case Access.Guest   => Result.internalError("unlinkCoi: guest user seen; should be impossible").pure[F]
          case Access.Ngo     => OdbError.NotAuthorized(user.id).asFailureF // not allowed
          case Access.Staff   |
               Access.Admin   |          
               Access.Service => unlinkUnconditionally(input) 
          case Access.Pi      =>
            session.prepareR(Statements.UnlinkCoiAsPi).use: pc =>
              pc.execute(user.id, input.programId, input.userId).map:
                case Completion.Delete(1) => Result(true)
                case Completion.Delete(0) => OdbError.NotAuthorized(user.id).asFailure
                case other                => Result.internalError(s"unlinkCoi: unexpected completion: $other")
        
      private def unlinkObserver(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] =
        user.role.access match
          case Access.Guest   => Result.internalError("unlinkObserver: guest user seen; should be impossible").pure[F]
          case Access.Ngo     => OdbError.NotAuthorized(user.id).asFailureF // not allowed
          case Access.Staff   |
               Access.Admin   |          
               Access.Service => unlinkUnconditionally(input) 
          case Access.Pi      =>
            session.prepareR(Statements.UnlinkObserverAsPiOrCoi).use: pc =>
              pc.execute(user.id, input.programId, input.userId).map:
                case Completion.Delete(1) => Result(true)
                case Completion.Delete(0) => OdbError.NotAuthorized(user.id).asFailure
                case other                => Result.internalError(s"unlinkCoi: unexpected completion: $other")

      private def unlinkNgoSupport(input: UnlinkUserInput, ptag: Tag)(using Transaction[F], Services.NgoAccess): F[Result[Boolean]] =
        user.role match
          case GuestRole             => Result.internalError("unlinkNgoSupport: guest user seen; should be impossible").pure[F]
          case Pi(_)                 => Result.internalError("unlinkNgoSupport: pi user seen; should be impossible").pure[F]
          case StandardRole.Staff(_) |
               StandardRole.Admin(_) |              
               ServiceRole(_)        => unlinkUnconditionally(input)
          case Ngo(_, partner)       =>
            if partner.tag === ptag.value then unlinkUnconditionally(input)
            else OdbError.NotAuthorized(user.id).asFailureF

      def unlinkUser(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] = {
        // Figure out how the specified user is linked to the specified program, then call the appropriate
        // unlink method (which will verify that the *calling* user is allowed to perform the unlink).
        val R  = ProgramUserRole
        val T  = ProgramUserSupportType
        val af = Statements.selectLinkType(user, input.programId, input.userId)
        session.prepareR(af.fragment.query(program_user_role *: program_user_support_type.opt *: tag.opt)).use: pq =>
          pq.option(af.argument).flatMap:
            case None                                           => Result(false).pure[F] // no such link, or not visible
            case Some((R.Coi, None, None))                      => unlinkCoi(input)
            case Some((R.CoiRO, None, None))                 => unlinkObserver(input)
            case Some((R.Support, Some(T.Staff), None))         => requireStaffAccess(unlinkUnconditionally(input))
            case Some((R.Support, Some(T.Partner), Some(ptag))) => requireNgoAccess(unlinkNgoSupport(input, ptag))
            case Some(other)                                    => Result.internalError(s"Nonsensical link type: $other").pure[F]
      }
      
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
              UpdateProgramsError.InvalidSemester(SET.semester).failure
          }

        (for {
          _   <- ResultT(setup.map(Result.apply))
          ids <- ResultT(updatePrograms)
        } yield ids).value

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

    def selectLinkType(user: User, pid: Program.Id, uid: User.Id): AppliedFragment =
      sql"""
        SELECT
          c_role,           
          c_support_type,   
          c_support_partner
        FROM t_program_user
        WHERE
          c_program_id = $program_id
        AND
          c_user_id = $user_id
      """.apply(pid, uid) |+| existsUserAccess(user, pid).foldMap(void"AND " |+| _)

    val UnlinkUnconditionally: Command[(Program.Id, User.Id)] =
      sql"""
        DELETE FROM
          t_program_user
        WHERE
          c_program_id = $program_id
        AND
          c_user_id = $user_id
      """.command

    val UnlinkCoiAsPi: Command[(User.Id, Program.Id, User.Id)] =
      sql"""
        DELETE FROM t_program_user u
        USING t_program p
        WHERE p.c_program_id = u.c_program_id
        AND   p.c_program_id = $program_id        
        AND   p.c_pi_user_id = $user_id
        AND   u.c_role = 'coi'
        AND   u.c_user_id = $user_id
      """
        .command
        .contramap((caller, pid, uid) => (pid, caller, uid))

    val UnlinkObserverAsPiOrCoi: Command[(User.Id, Program.Id, User.Id)] =
      sql"""
        DELETE FROM t_program_user u
        USING t_program p
        WHERE p.c_program_id = u.c_program_id
        AND   p.c_program_id = $program_id        
        AND   (p.c_pi_user_id = $user_id OR exists(SELECT * FROM t_program_user x WHERE x.c_role = 'coi' AND x.c_user_id = $user_id))
        AND   u.c_role = 'observer'
        AND   u.c_user_id = $user_id
      """
        .command
        .contramap((caller, pid, uid) => (pid, caller, caller, uid))

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
          c_program_id
        )
        ON COMMIT DROP
          AS SELECT
            which.pid
        FROM (""" |+| whichProgramIds |+| void""") AS which (pid)
        INNER JOIN t_program prog
          ON prog.c_program_id = which.pid
      """

    def updates(SET: ProgramPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.name.map(sql"c_name = $text_nonempty"),
          SET.semester.map(sql"c_semester = $semester"),
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
        case Pi(_)                 => (void"(" |+| existsUserAsPi(programId, user.id) |+| void" OR " |+| existsUserAsCoi(programId, user.id) |+| void")").some
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
      val up = LinkUser(targetProgram, targetUser, ProgramUserRole.CoiRO, None, None)
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
