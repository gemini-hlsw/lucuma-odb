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
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Access
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Guest
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.GuestRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProgramReference.Description
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
import lucuma.odb.graphql.input.ProgramUserPropertiesInput
import lucuma.odb.graphql.input.SetProgramReferenceInput
import lucuma.odb.graphql.input.UnlinkUserInput
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

  def setProgramReference(input: SetProgramReferenceInput)(using Transaction[F], Services.StaffAccess): F[Result[(Program.Id, Option[ProgramReference])]]

  /**
   * Insert a new program, where the calling user becomes PI (unless it's a Service user, in which
   * case the PI is left empty.
   */
  def insertProgram(SET: Option[ProgramPropertiesInput.Create])(using Transaction[F]): F[Program.Id]

  /**
   * Insert a new calibration program, PI is left empty.
   */
  def insertCalibrationProgram(SET: Option[ProgramPropertiesInput.Create], calibrationRole: CalibrationRole, description: Description)(using Transaction[F]): F[Program.Id]

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

  def updateProgramUsers(SET: ProgramUserPropertiesInput, which: AppliedFragment)(using Transaction[F]): F[Result[List[(Program.Id, User.Id)]]]

}

object ProgramService {

  sealed trait LinkUserRequest:
    def programId: Program.Id
    def userId: User.Id

  object LinkUserRequest:
    case class Coi(programId: Program.Id, partnerLink: PartnerLink, userId: User.Id)   extends LinkUserRequest
    case class CoiRo(programId: Program.Id, partnerLink: PartnerLink, userId: User.Id) extends LinkUserRequest
    case class Support(programId: Program.Id, userId: User.Id) extends LinkUserRequest

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

      override def setProgramReference(input: SetProgramReferenceInput)(using Transaction[F], Services.StaffAccess): F[Result[(Program.Id, Option[ProgramReference])]] = {
        def validateProposal(pid: Program.Id): F[Result[Unit]] =
          proposalService.hasProposal(pid).map { hasProposal =>
            OdbError
              .InvalidProgram(pid, s"Cannot set the program reference for $pid to ${input.SET.programType.abbreviation} until its proposal is removed.".some)
              .asFailure
              .whenA(hasProposal && input.SET.programType != ProgramType.Science)
          }

        programService.resolvePid(input.programId,input.proposalReference, input.programReference).flatMap: r =>
          r.flatTraverse: pid =>
            (for {
              _ <- ResultT(validateProposal(pid))
              r <- ResultT(setProgramReferenceImpl(pid, input.SET).map(_.map((pid, _))))
            } yield r).value
      }

      def insertProgram(SET: Option[ProgramPropertiesInput.Create])(using Transaction[F]): F[Program.Id] =
        Trace[F].span("insertProgram") {
          val SETʹ = SET.getOrElse(ProgramPropertiesInput.Create.Empty)

          session
            .prepareR(Statements.InsertProgram)
            .use(_.unique(SETʹ.name))
            .flatTap { pid =>
              user match {
                case ServiceUser(_, _) =>
                  Concurrent[F].unit
                case nonServiceUser    =>
                  // Link the PI to the program.
                  session.executeCommand(
                    Statements.LinkUser(pid, user.id, UserType.fromUser(user), ProgramUserRole.Pi, PartnerLink.HasUnspecifiedPartner)
                  ).void
              }
            }
        }

      def linkUserImpl(req: ProgramService.LinkUserRequest)(using Transaction[F]): F[LinkUserResponse] = {
        val af: Option[AppliedFragment] =
          req match {
            case LinkUserRequest.Coi(programId, partnerLink, userId) => Statements.linkCoi(programId, userId, partnerLink, user)
            case LinkUserRequest.CoiRo(programId, partnerLink, userId) => Statements.linkCoiReadOnly(programId, userId, partnerLink, user)
            case LinkUserRequest.Support(programId, userId) => Statements.linkSupport(programId, userId, user)
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

      def insertCalibrationProgram(SET: Option[ProgramPropertiesInput.Create], calibrationRole: CalibrationRole, description: Description)(using Transaction[F]): F[Program.Id] =
        Trace[F].span("insertCalibrationProgram") {
          val SETʹ = SET.getOrElse(ProgramPropertiesInput.Create.Empty)

          session.prepareR(Statements.InsertCalibrationProgram).use(_.unique(SETʹ.name, calibrationRole, description.value))
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

      def unlinkUser(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] = {
        // Figure out how the specified user is linked to the specified program, then call the appropriate
        // unlink method (which will verify that the *calling* user is allowed to perform the unlink).
        val R  = ProgramUserRole
        val af = Statements.selectLinkType(user, input.programId, input.userId)
        session.prepareR(af.fragment.query(program_user_role)).use: pq =>
          pq.option(af.argument).flatMap:
            case None             => Result(false).pure[F] // no such link, or not visible
            case Some(R.Pi)       => OdbError.InvalidArgument("Cannot unlink the PI".some).asFailureF
            case Some(R.Coi)      => unlinkCoi(input)
            case Some(R.CoiRO)    => unlinkObserver(input)
            case Some(R.Support)  => requireStaffAccess(unlinkUnconditionally(input))
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
          }

        (for {
          _   <- ResultT(setup.map(Result.apply))
          ids <- ResultT(updatePrograms)
        } yield ids).value

      }

      override def updateProgramUsers(
        SET:   ProgramUserPropertiesInput,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[List[(Program.Id, User.Id)]]] =
        Statements.updateProgramUsers(user, SET, which).fold(Nil.success.pure[F]) { af =>
          session.prepareR(af.fragment.query(program_id *: user_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.success)
          }
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
          c_role
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
        DELETE FROM t_program_user
        WHERE c_program_id = $program_id
        AND   c_user_id    = $user_id
        AND   c_role       = 'coi'
        AND EXISTS (
          SELECT 1
          FROM t_program_user u
          WHERE u.c_program_id = $program_id
          AND   u.c_user_id = $user_id
          AND   u.c_role = 'pi'
        )
      """
        .command
        .contramap((caller, pid, uid) => (pid, uid, pid, caller))

    val UnlinkObserverAsPiOrCoi: Command[(User.Id, Program.Id, User.Id)] =
      sql"""
        DELETE FROM t_program_user
        WHERE c_program_id = $program_id
        AND   c_user_id    = $user_id
        AND   c_role       = 'coi_ro'
        AND EXISTS (
          SELECT 1
          FROM t_program_user u
          WHERE u.c_program_id = $program_id
          AND   u.c_user_id    = $user_id
          AND   (u.c_role = 'pi' OR u.c_role = 'coi')
        )
      """
        .command
        .contramap((caller, pid, uid) => (pid, uid, pid, caller))

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
          SET.name.map(sql"c_name = $text_nonempty")
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

    def updateProgramUsers(
      user:  User,
      SET:   ProgramUserPropertiesInput,
      which: AppliedFragment
    ): Option[AppliedFragment] = {
      val alias = "o"

      val ups = NonEmptyList.fromList(
        SET.partnerLink.toList.flatMap { pl => List(
          sql"c_partner_link = $partner_link_type"(pl.linkType),
          sql"c_partner      = ${partner.opt}"(pl.partnerOption)
        )}
      )

      ups.map { nel =>
        val up =
          sql"""
            UPDATE t_program_user AS #$alias
            SET """(Void) |+| nel.intercalate(void", ") |+| void" " |+|
          sql"WHERE (#$alias.c_program_id, #$alias.c_user_id) IN ("(Void) |+| which |+| void")"

        (correlatedExistsUserAccess(user, alias, "i").fold(up) { exists =>
          up |+| void" AND " |+| exists
        }) |+| sql" RETURNING #$alias.c_program_id, #$alias.c_user_id"(Void)
      }
    }

    // This is the same as `existsUserAs` but where the program is
    // correlated to an outer query program instead of provided as a parameter.
    private def correlatedExistsUserAs(
      userId:     User.Id,
      outerAlias: String,
      innerAlias: String,
      role:       ProgramUserRole
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_program_user #$innerAlias where #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_user_id = $user_id and #$innerAlias.c_role = $program_user_role)
      """.apply(userId, role)

    def existsUserAs(
      programId: Program.Id,
      userId: User.Id,
      role: ProgramUserRole
    ): AppliedFragment =
      sql"""
        EXISTS (select c_role from t_program_user where c_program_id = $program_id and c_user_id = $user_id and c_role = $program_user_role)
      """.apply(programId, userId, role)

    def existsUserAsPi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsUserAs(programId, userId, ProgramUserRole.Pi)

    def existsUserAsCoi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsUserAs(programId, userId, ProgramUserRole.Coi)

    // This is the same as `existsAllocationForPartner` but where the program is
    // correlated to an outer query program instead of provided as a parameter.
    private def correlatedExistsAllocationForPartner(
      outerAlias: String,
      innerAlias: String
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_allocation WHERE #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_ta_category=#$outerAlias.c_partner and #$innerAlias.c_duration > 'PT')
      """(Void)

    def existsAllocationForPartner(
      programId: Program.Id,
      partner: Partner
    ): AppliedFragment =
      sql"""
        EXISTS (select c_duration from t_allocation where c_program_id = $program_id and c_ta_category=${lucuma.odb.util.Codecs.time_accounting_category} and c_duration > 'PT')
      """.apply(programId, partner.timeAccountingCategory)

    def existsUserAccess(
      user:      User,
      programId: Program.Id
    ): Option[AppliedFragment] =
      user.role match {
        case GuestRole             => existsUserAsPi(programId, user.id).some
        case Pi(_)                 => (void"(" |+| existsUserAsPi(programId, user.id) |+| void" OR " |+| existsUserAsCoi(programId, user.id) |+| void")").some
        case Ngo(_, partner)       => existsAllocationForPartner(programId, partner).some
        case ServiceRole(_) |
             StandardRole.Admin(_) |
             StandardRole.Staff(_) => none
      }

    // This is the same as `existsUserAccess` but where the program is
    // correlated to an outer query program instead of provided as a parameter.
    private def correlatedExistsUserAccess(
      user:       User,
      outerAlias: String,
      innerAlias: String
    ): Option[AppliedFragment] =
      user.role match {
        case GuestRole             => correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi).some
        case Pi(_)                 => (void"(" |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi) |+| void" OR " |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Coi) |+| void")").some
        case Ngo(_, partner)       => correlatedExistsAllocationForPartner(outerAlias, innerAlias).some
        case ServiceRole(_)        |
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
    val InsertProgram: Query[Option[NonEmptyString], Program.Id] =
      sql"""
        INSERT INTO t_program (c_name)
        VALUES (${text_nonempty.opt})
        RETURNING c_program_id
      """.query(program_id)

    /** Insert a calibration program, without a user for a staff program */
    val InsertCalibrationProgram: Query[(Option[NonEmptyString], CalibrationRole, String), Program.Id] =
      sql"""
        INSERT INTO t_program (c_name, c_calibration_role, c_library_desc)
        VALUES (${text_nonempty.opt}, $calibration_role, $text)
        RETURNING c_program_id
      """.query(program_id)

    /** Link a user to a program, without any access checking. */
    val LinkUser: Fragment[(Program.Id, User.Id, UserType, ProgramUserRole, PartnerLink)] =
      sql"""
        INSERT INTO t_program_user (c_program_id, c_user_id, c_user_type, c_role, c_partner_link, c_partner)
        SELECT $program_id, $user_id, $user_type, $program_user_role, $partner_link
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
      partnerLink: PartnerLink,
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, ProgramUserRole.Coi, partnerLink)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, partner))
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
    def linkCoiReadOnly(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      partnerLink: PartnerLink,
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, ProgramUserRole.CoiRO, partnerLink)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, partner))
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
    def linkSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, ProgramUserRole.Support, PartnerLink.HasUnspecifiedPartner)
      user.role.access match {
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this
      }
    }

  }

}
