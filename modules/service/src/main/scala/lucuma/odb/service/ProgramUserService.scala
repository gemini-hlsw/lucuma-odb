// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.UserType
import lucuma.odb.graphql.input.AddProgramUserInput
import lucuma.odb.graphql.input.ChangeProgramUserRoleInput
import lucuma.odb.graphql.input.LinkUserInput
import lucuma.odb.graphql.input.ProgramUserPropertiesInput
import lucuma.odb.util.Codecs.educational_status
import lucuma.odb.util.Codecs.gender
import lucuma.odb.util.Codecs.partner
import lucuma.odb.util.Codecs.partner_link
import lucuma.odb.util.Codecs.partner_link_type
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.program_user_id
import lucuma.odb.util.Codecs.program_user_role
import lucuma.odb.util.Codecs.user_id
import lucuma.odb.util.Codecs.user_type
import lucuma.odb.util.Codecs.varchar_nonempty
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.varchar
import skunk.data.Completion
import skunk.syntax.all.*

import Services.Syntax.*

trait ProgramUserService[F[_]]:

  /**
   * Adds a program user that is not yet linked to an actual authenticated user.
   * @param input program, role that the user plays, and user properties
   * @return new program user id
   */
  def addProgramUser(
    input: AddProgramUserInput
  )(using Transaction[F]): F[Result[ProgramUser.Id]]

  def deleteProgramUser(
    id: ProgramUser.Id
  )(using Transaction[F]): F[Result[Boolean]]

  def changeProgramUserRole(
    input: ChangeProgramUserRoleInput
  )(using Transaction[F]): F[Result[ProgramUser.Id]]

  /**
   * Updates program user properties such as fallback profile, thesis and
   * gender.
   *
   * @param SET property edits to apply
   * @param which list of program user ids to update
   * @return updated program user ids
   */
  def updateProperties(
    SET:   ProgramUserPropertiesInput,
    which: AppliedFragment
  )(using Transaction[F]): F[Result[List[ProgramUser.Id]]]

  def selectLinkData(
    id: ProgramUser.Id
  )(using Transaction[F]): F[Result[(Program.Id, ProgramUserRole, Option[User.Id])]]

  /**
   * Sets the program user / user link, assuming it is not currently set and
   * the current user has privileges to do so.
   */
  def linkUser(
    input: LinkUserInput
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Unlinks the user associated with the given program user, if any, and
   * returns their user id.
   */
  def unlinkUser(
    id: ProgramUser.Id
  )(using Transaction[F]): F[Result[Option[User.Id]]]

  /**
   * Links the current user to the given program user, assuming it is not
   * currently set.  Performs no access checking, as the assumption is that the
   * using being linked is not yet part of the program and has no privileges.
   */
  def linkInvitationAccept(
    id: ProgramUser.Id
  )(using Transaction[F]): F[Result[Unit]]

  /**
   * Adds the current user as the PI for given program.
   */
  def addAndLinkPi(
    programId: Program.Id
  )(using Transaction[F]): F[ProgramUser.Id]

  /** Check to see if the user has read access to the given program. */
  def userHasReadAccess(
    programId: Program.Id
  )(using Transaction[F]): F[Boolean]

  /** Check to see if the user has write access to the given program. */
  def userHasWriteAccess(
    programId: Program.Id
  )(using Transaction[F]): F[Boolean]

object ProgramUserService:
  def instantiate[F[_]: Concurrent](using Services[F]): ProgramUserService[F] =
    new ProgramUserService[F]:

      override def selectLinkData(
        id: ProgramUser.Id
      )(using Transaction[F]): F[Result[(Program.Id, ProgramUserRole, Option[User.Id])]] =
        session
          .option(Statements.SelectLinkData)(id)
          .map(Result.fromOption(_, OdbError.InvalidProgramUser(id).asProblem))

      override def addAndLinkPi(
        programId: Program.Id
      )(using Transaction[F]): F[ProgramUser.Id] =
        session.unique(Statements.InsertAndLinkUnconditionally)(
          programId, user.id, UserType.fromUser(user), ProgramUserRole.Pi, PartnerLink.HasUnspecifiedPartner
        )

      override def addProgramUser(
        input: AddProgramUserInput
      )(using Transaction[F]): F[Result[ProgramUser.Id]] =
        val set0 = input.SET.getOrElse(ProgramUserPropertiesInput.Empty)
        val link = set0.partnerLink.getOrElse(PartnerLink.HasUnspecifiedPartner)

        val insert =
          Statements
            .insertProgramUser(input.role, input.programId, link, user)
            .flatTraverse: af =>
              val stmt = sql"${af.fragment} RETURNING c_program_user_id".query(program_user_id)
              session.prepare(stmt).flatMap: pq =>
                pq.option(af.argument).map(_.success)

        (for
          o  <- ResultT(insert)
          id <- o.fold(ResultT.fromResult(OdbError.NotAuthorized(user.id).asFailure))(ResultT.pure)
          set = ProgramUserPropertiesInput.partnerLink.replace(link.some)(set0)
          _  <- ResultT(updateProperties(set, sql"$program_user_id"(id)))
        yield id).value

      override def changeProgramUserRole(
        input: ChangeProgramUserRoleInput
      )(using Transaction[F]): F[Result[ProgramUser.Id]] =
        session
          .prepare(Statements.SelectLinkData)
          .flatMap(_.option(input.programUserId))
          .flatMap:
            case None                     =>
              OdbError.InvalidProgramUser(input.programUserId, s"ProgramUser ${input.programUserId} was not found.".some).asFailureF
            case Some((pid, prevRole, _)) =>
              Statements
                .changeProgramUserRole(input.programUserId, prevRole, input.newRole, pid, user)
                .flatTraverse: af =>
                  session.prepare(af.fragment.command).flatMap: pq =>
                    pq.execute(af.argument).map:
                      case Completion.Update(1) => input.programUserId.success
                      case _                    => OdbError.NotAuthorized(user.id).asFailure


      override def deleteProgramUser(
        id: ProgramUser.Id
      )(using Transaction[F]): F[Result[Boolean]] =
        session
          .prepare(Statements.SelectLinkData)
          .flatMap(_.option(id))
          .flatMap:
            case None                 => false.success.pure
            case Some((pid, role, _)) =>
              Statements
                .deleteProgramUser(id, role, pid, user)
                .flatTraverse: af =>
                  session.prepare(af.fragment.command).flatMap: pq =>
                    pq.execute(af.argument).map:
                      case Completion.Delete(1) => true.success
                      case _                    => OdbError.NotAuthorized(user.id).asFailure

      override def updateProperties(
        SET:   ProgramUserPropertiesInput,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[List[ProgramUser.Id]]] =
        Statements.updateProgramUsers(user, SET, which).fold(Nil.success.pure[F]): af =>
          session.prepareR(af.fragment.query(program_user_id)).use: pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.success)

      override def linkInvitationAccept(
        id: ProgramUser.Id
      )(using Transaction[F]): F[Result[Unit]] =
        linkUserImpl(id, user.id, (_, _) => none.success)

      override def linkUser(
        input: LinkUserInput
      )(using Transaction[F]): F[Result[Unit]] =
        linkUserImpl(
          input.programUserId,
          input.userId,
          (role, pid) => Statements.accessCheck("add", role, pid, user)
        )

      @annotation.nowarn("msg=unused implicit parameter")
      private def linkUserImpl(
        targetProgramUserId: ProgramUser.Id,
        targetUserId:        User.Id,
        accessCheck:         (ProgramUserRole, Program.Id) => Result[Option[AppliedFragment]]
      )(using Transaction[F]): F[Result[Unit]] =
        session.prepare(Statements.SelectLinkData).flatMap(_.option(targetProgramUserId)).flatMap:
          case None              =>
            OdbError.InvalidProgramUser(targetProgramUserId).asFailureF
          case Some((pid, role, Some(uid))) if uid === targetUserId =>
            val msg =
              if targetUserId === user.id then "You are already in the specified role; no action taken."
              else s"The user $uid is already linked to program $pid; no action taken."
            OdbError.NoAction(msg.some).asWarningF(())
          case Some((pid, role, Some(uid))) =>
            OdbError.InvalidUser(targetUserId, s"The user $uid must be unlinked before $targetUserId may be linked.".some).asFailureF
          case Some((pid, role, None)) =>
            accessCheck(role, pid)
              .map(Statements.linkStandardUser(targetProgramUserId, targetUserId, _))
              .flatTraverse: af =>
                val stmt = sql"${af.fragment}".command
                session.prepare(stmt).flatMap: pq =>
                  pq.execute(af.argument)
                    .map:
                      case Completion.Update(1) => ().success
                      case a                    => OdbError.NotAuthorized(user.id).asFailure
                    .recover:
                      case SqlState.UniqueViolation(_)     =>
                        OdbError.NoAction(s"User $targetUserId is already linked to program $pid.".some).asFailure
                      case SqlState.ForeignKeyViolation(_) =>
                        OdbError.InvalidUser(targetUserId, s"User $targetUserId does not exist or is of a nonstandard type.".some).asFailure

      override def unlinkUser(
        mid: ProgramUser.Id
      )(using Transaction[F]): F[Result[Option[User.Id]]] =
        session.prepare(Statements.SelectLinkData).flatMap(_.option(mid)).flatMap:
          case None                         => OdbError.InvalidProgramUser(mid).asFailureF
          case Some((pid, role, None))      => none.success.pure
          case Some((pid, role, Some(uid))) =>
            Statements.accessCheck("unlink", role, pid, user)
              .map(Statements.unlinkStandardUser(mid, _))
              .flatTraverse: af =>
                val stmt = sql"${af.fragment}".command
                session.prepare(stmt).flatMap: pq =>
                  pq.execute(af.argument)
                    .map:
                      case Completion.Update(1) => uid.some.success
                      case a                    => OdbError.NotAuthorized(user.id).asFailure

      override def userHasReadAccess(
        programId: Program.Id
      )(using Transaction[F]): F[Boolean] =
        Statements.existsUserReadAccess(user, programId).fold(true.pure[F]): af =>
          val stmt = sql"SELECT ${af.fragment}".query(bool)
          session.prepareR(stmt).use: pg =>
            pg.unique(af.argument)

      override def userHasWriteAccess(
        programId: Program.Id
      )(using Transaction[F]): F[Boolean] =
        Statements.existsUserWriteAccess(user, programId).fold(true.pure[F]): af =>
          val stmt = sql"SELECT ${af.fragment}".query(bool)
          session.prepareR(stmt).use: pg =>
            pg.unique(af.argument)

  end instantiate

  object Statements:

    val InsertAndLinkUnconditionally: Query[(Program.Id, User.Id, UserType, ProgramUserRole, PartnerLink), ProgramUser.Id] =
      sql"""
        INSERT INTO t_program_user (
          c_program_id,
          c_user_id,
          c_user_type,
          c_role,
          c_partner_link,
          c_partner
        ) SELECT $program_id, $user_id, $user_type, $program_user_role, $partner_link
        RETURNING c_program_user_id
      """.query(program_user_id)

    def insertProgramUser(
      targetRole:        ProgramUserRole,
      targetProgramId:   Program.Id,
      targetPartnerLink: PartnerLink,
      sourceUser:        User  // user doing the program user update
    ): Result[AppliedFragment] =
      accessCheck("add", targetRole, targetProgramId, sourceUser)
        .map:
          case None     => AppliedFragment.empty
          case Some(ac) => void" WHERE " |+| ac
        .map: ac =>
          sql"""
            INSERT INTO t_program_user(
              c_program_id,
              c_role,
              c_partner_link,
              c_partner
            )
            SELECT $program_id, $program_user_role, $partner_link"""(
              targetProgramId, targetRole, targetPartnerLink
            ) |+| ac

    def changeProgramUserRole(
      targetProgramUserId: ProgramUser.Id,
      prevRole:            ProgramUserRole,
      nextRole:            ProgramUserRole,
      programId:           Program.Id,
      sourceUser:          User
    ): Result[AppliedFragment] =
      (accessCheck("unassign", prevRole, programId, sourceUser),
       accessCheck("assign",   nextRole, programId, sourceUser)
      )
        .mapN:
          case (None,    None   ) => AppliedFragment.empty
          case (None,    Some(a)) => void" AND " |+| a
          case (Some(u), None   ) => void" AND " |+| u
          case (Some(u), Some(a)) => void" AND " |+| u |+| void" AND " |+| a
        .map: ac =>
          sql"""
            UPDATE t_program_user
               SET c_role = $program_user_role
             WHERE c_program_user_id = $program_user_id
          """(nextRole, targetProgramUserId) |+| ac

    def deleteProgramUser(
      pui:             ProgramUser.Id,
      targetRole:      ProgramUserRole,
      targetProgramId: Program.Id,
      sourceUser:      User            // user doing the program user update
    ): Result[AppliedFragment] =
      accessCheck("delete", targetRole, targetProgramId, sourceUser)
        .map:
          case None     => AppliedFragment.empty
          case Some(ac) => void" AND " |+| ac
        .map: ac =>
          sql"""
            DELETE FROM t_program_user
          WHERE c_program_user_id = $program_user_id
          """(pui) |+| ac

    val SelectLinkData: Query[ProgramUser.Id, (Program.Id, ProgramUserRole, Option[User.Id])] =
      sql"""
        SELECT
          c_program_id,
          c_role,
          c_user_id
        FROM t_program_user
        WHERE
          c_program_user_id = $program_user_id
      """.query(program_id *: program_user_role *: user_id.opt)

    /**
     * Access checks for adding/updating a COI program user.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI.
     */
    private def isPiOrBetter(
      action:          String,
      targetProgramId: Program.Id,
      sourceUser:      User  // user doing the program user update
    ): Result[Option[AppliedFragment]] =
      sourceUser.role match
        case GuestRole                    => OdbError.NotAuthorized(sourceUser.id, s"Guest users may not $action CoIs.".some).asFailure
        case ServiceRole(_)               => none[AppliedFragment].success
        case StandardRole.Admin(_)        => none[AppliedFragment].success
        case StandardRole.Ngo(_, partner) => existsAllocationForPartner(targetProgramId, partner).some.success
        case StandardRole.Pi(_)           => existsUserAsPi(targetProgramId, sourceUser.id).some.success
        case StandardRole.Staff(_)        => none[AppliedFragment].success

    /**
     * Access checks for adding/updating a COI RO program user.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI or Coi.
     */
    private def isCoiOrBetter(
      action:          String,
      targetProgramId: Program.Id,
      sourceUser:      User  // user doing the program user update
    ): Result[Option[AppliedFragment]] =
      sourceUser.role match
        case GuestRole                    => OdbError.NotAuthorized(sourceUser.id, s"Guest users may not $action CoIs.".some).asFailure
        case ServiceRole(_)               => none[AppliedFragment].success
        case StandardRole.Admin(_)        => none[AppliedFragment].success
        case StandardRole.Ngo(_, partner) => existsAllocationForPartner(targetProgramId, partner).some.success
        case StandardRole.Staff(_)        => none[AppliedFragment].success
        case StandardRole.Pi(_)           =>
          Some(
            void"("                                         |+|
            existsUserAsPi(targetProgramId, sourceUser.id)  |+|
            void" OR "                                      |+|
            existsUserAsCoi(targetProgramId, sourceUser.id) |+|
            void")"
          ).success

    /**
     * Access checks for adding/updating a support user.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    private def isStaffOrBetter(
      action:     String,
      sourceUser: User // user doing the program user update
    ): Result[Option[AppliedFragment]] =
      import Access.{Admin, Staff, Service}
      sourceUser.role.access match
        case Admin | Staff | Service => none[AppliedFragment].success // ok
        case _                       => OdbError.NotAuthorized(sourceUser.id, s"Only admin, staff or service users may $action support users.".some).asFailure

    /**
     * Access checks for adding/updating a program user.  What the user is
     * allowed to do will differ depending on type of program user (its role)
     * and the role of the user attempting the update.
     */
    def accessCheck(
      action:          String,
      targetRole:      ProgramUserRole,
      targetProgramId: Program.Id,
      sourceUser:      User  // user doing the program user update
    ): Result[Option[AppliedFragment]] =
      targetRole match
        case ProgramUserRole.Pi               => OdbError.UpdateFailed("PIs are fixed at program creation time.".some).asFailure
        case ProgramUserRole.Coi              => isPiOrBetter(action, targetProgramId, sourceUser)
        case ProgramUserRole.CoiRO          |
             ProgramUserRole.External         => isCoiOrBetter(action, targetProgramId, sourceUser)
        case ProgramUserRole.SupportPrimary |
             ProgramUserRole.SupportSecondary => isStaffOrBetter(action, sourceUser)

    def linkStandardUser(
      targetId:     ProgramUser.Id,
      targetUserId: User.Id,
      accessCheck:  Option[AppliedFragment]
    ): AppliedFragment =
      sql"""
        UPDATE t_program_user
           SET c_user_id   = $user_id,
               c_user_type = 'standard'::e_user_type
         WHERE c_program_user_id = $program_user_id
           AND c_user_id IS NULL
      """(targetUserId, targetId) |+|
      accessCheck.map(ac => void" AND " |+| ac).getOrElse(AppliedFragment.empty)

    def unlinkStandardUser(
      targetId:    ProgramUser.Id,
      accessCheck: Option[AppliedFragment]
    ): AppliedFragment =
      sql"""
        UPDATE t_program_user
           SET c_user_id   = null,
               c_user_type = 'standard'::e_user_type
         WHERE c_program_user_id = $program_user_id
      """(targetId) |+|
      accessCheck.map(ac => void" AND " |+| ac).getOrElse(AppliedFragment.empty)

    def existsProgramUserInRole(
      programId: Program.Id,
      userId: User.Id,
      acceptableRoles: List[ProgramUserRole]
    ): AppliedFragment =
      val roles: Encoder[acceptableRoles.type] = program_user_role.list(acceptableRoles)
      sql"""
        EXISTS (select c_role from t_program_user where c_program_id = $program_id and c_user_id = $user_id and c_role IN ($roles))
      """.apply(programId, userId, acceptableRoles)

    def existsUserAsPi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsProgramUserInRole(programId, userId, List(ProgramUserRole.Pi))

    def existsUserAsCoi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsProgramUserInRole(programId, userId, List(ProgramUserRole.Coi))

    def existsUserAsCoiRO(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsProgramUserInRole(programId, userId, List(ProgramUserRole.CoiRO))

    def existsAllocationForPartner(
      programId: Program.Id,
      partner: Partner
    ): AppliedFragment =
      sql"""
        EXISTS (select c_duration from t_allocation where c_program_id = $program_id and c_ta_category=${lucuma.odb.util.Codecs.time_accounting_category} and c_duration > 'PT')
      """.apply(programId, partner.timeAccountingCategory)

    def existsUserReadAccess(
      user:      User,
      programId: Program.Id
    ): Option[AppliedFragment] =
      user.role match
        case GuestRole                    => existsUserAsPi(programId, user.id).some
        case StandardRole.Pi(_)           => (void"(" |+| existsProgramUserInRole(programId, user.id, List(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.CoiRO, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary)) |+| void")").some
        case StandardRole.Ngo(_, partner) => existsAllocationForPartner(programId, partner).some
        case ServiceRole(_) |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)        => none

    def existsUserWriteAccess(
      user:      User,
      programId: Program.Id
    ): Option[AppliedFragment] =
      user.role match
        case GuestRole                    => existsUserAsPi(programId, user.id).some
        case StandardRole.Pi(_)           => (void"(" |+| existsProgramUserInRole(programId, user.id, List(ProgramUserRole.Pi, ProgramUserRole.Coi, ProgramUserRole.SupportPrimary, ProgramUserRole.SupportSecondary)) |+| void")").some
        case StandardRole.Ngo(_, partner) => existsAllocationForPartner(programId, partner).some
        case ServiceRole(_) |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)        => none

    def whereUserReadAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"WHERE " |+| af
      }

    def whereUserWriteAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserWriteAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"WHERE " |+| af
      }

    def andWhereUserReadAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"AND " |+| af
      }

    def andWhereUserWriteAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserWriteAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"AND " |+| af
      }

    private def correlatedExistsUserAs(
      userId:     User.Id,
      outerAlias: String,
      innerAlias: String,
      role:       ProgramUserRole
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_program_user #$innerAlias where #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_user_id = $user_id and #$innerAlias.c_role = $program_user_role)
      """.apply(userId, role)

    private def correlatedExistsAllocationForPartner(
      outerAlias: String,
      innerAlias: String
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_allocation #$innerAlias WHERE #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_ta_category=#$outerAlias.c_partner and #$innerAlias.c_duration > 'PT')
      """(Void)

    private def correlatedExistsUserAccess(
      user:       User,
      outerAlias: String,
      innerAlias: String
    ): Option[AppliedFragment] =
      user.role match
        case GuestRole                    => correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi).some
        case StandardRole.Pi(_)           => (void"(" |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi) |+| void" OR " |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Coi) |+| void")").some
        case StandardRole.Ngo(_, partner) => correlatedExistsAllocationForPartner(outerAlias, innerAlias).some
        case ServiceRole(_)        |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)        => none

    private def correlatedPiAccessOnly(
      user:       User,
      outerAlias: String,
      innerAlias: String
    ): Option[AppliedFragment] =
      user.role match
        case ServiceRole(_)        |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)   => none
        case _                       => correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi).some

    def updateProgramUsers(
      user:  User,
      SET:   ProgramUserPropertiesInput,
      which: AppliedFragment
    ): Option[AppliedFragment] =

      val alias = "o"

      val upGivenName         = sql"c_fallback_given_name  = ${varchar.opt}"
      val upFamilyName        = sql"c_fallback_family_name = ${varchar.opt}"
      val upCreditName        = sql"c_fallback_credit_name = ${varchar.opt}"
      val upEmail             = sql"c_fallback_email       = ${varchar.opt}"

      val upEducationalStatus = sql"c_educational_status   = ${educational_status.opt}"
      val upThesis            = sql"c_thesis               = ${bool.opt}"
      val upGender            = sql"c_gender               = ${gender.opt}"
      val upAffiliation       = sql"c_affiliation          = ${varchar_nonempty.opt}"
      val upDataAccess        = sql"c_has_data_access      = $bool"

      val ups: Option[NonEmptyList[AppliedFragment]] = NonEmptyList.fromList(
        List(
          SET.fallbackProfile.flatMap(_.givenName).foldPresent(upGivenName),
          SET.fallbackProfile.flatMap(_.familyName).foldPresent(upFamilyName),
          SET.fallbackProfile.flatMap(_.creditName).foldPresent(upCreditName),
          SET.fallbackProfile.flatMap(_.email).foldPresent(upEmail),
          SET.educationalStatus.foldPresent(upEducationalStatus),
          SET.thesis.foldPresent(upThesis),
          SET.gender.foldPresent(upGender),
          SET.affiliation.foldPresent(upAffiliation),
          SET.hasDataAccess.map(upDataAccess)
        ).flattenOption :::
        SET.partnerLink.toList.flatMap { pl => List(
          sql"c_partner_link = $partner_link_type"(pl.linkType),
          sql"c_partner      = ${partner.opt}"(pl.partnerOption)
        )}
      )

      ups.map: nel =>
        val up =
          sql"""
            UPDATE t_program_user AS #$alias
            SET """(Void) |+| nel.intercalate(void", ") |+| void" " |+|
          sql"WHERE #$alias.c_program_user_id IN ("(Void) |+| which |+| void")"

        // If updating the data access flag, you must be the PI (or staff).
        // Otherwise normal access rules apply with the caveat that a user can
        // update their own record.
        val access = (SET.hasDataAccess *> correlatedPiAccessOnly(user, alias, "i")).orElse:
                       correlatedExistsUserAccess(user, alias, "i").map: ac =>
                         void" ("                                     |+|
                           sql"#$alias.c_user_id = $user_id"(user.id) |+| // updating our own user
                           void" OR "                                 |+|
                           ac                                         |+|
                         void")"

        (access.fold(up): exists =>
          up |+| void" AND " |+| exists
        ) |+| sql" RETURNING #$alias.c_program_user_id"(Void)

    end updateProgramUsers
  end Statements
