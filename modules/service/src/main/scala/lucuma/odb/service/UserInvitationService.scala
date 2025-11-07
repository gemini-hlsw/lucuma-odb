// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.model.ServiceRole
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.UserInvitation
import lucuma.odb.Config
import lucuma.odb.data.EmailId
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.graphql.input.RedeemUserInvitationInput
import lucuma.odb.graphql.input.RevokeUserInvitationInput
import lucuma.odb.util.Codecs.*
import skunk.Query
import skunk.SqlState
import skunk.Transaction
import skunk.codec.all.*
import skunk.syntax.all.*

import Services.Syntax.*

trait UserInvitationService[F[_]]:

  /** Create an invitation to join a program in a specified role. This current user must be the program's PI. */
  def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]]

  /** Redeem an invitation. */
  def redeemUserInvitation(input: RedeemUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]]

  /** Revoke an invitation. */
  def revokeUserInvitation(input: RevokeUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]]

object UserInvitationService:

  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email)(using Services[F]): UserInvitationService[F] =
    new UserInvitationService[F]:

      def sendInvitation(
        input:      CreateUserInvitationInput,
        invitation: UserInvitation,
        pid:        Program.Id,
        targetRole: ProgramUserRole
      )(using Transaction[F]): F[Result[EmailId]] = {

        val (action, preposition) = targetRole match
          case ProgramUserRole.External => ("share data", "from")
          case _                        => ("collaborate", "on")

        val subject: NonEmptyString = NonEmptyString.unsafeFrom(
          s"Invitation to $action from ${user.displayName}"
        )

        val textMessage: NonEmptyString = NonEmptyString.unsafeFrom(
          s"""${user.displayName} has invited you to $action $preposition a Gemini proposal. To accept this invitation go to ${emailConfig.exploreUrl} to log in, then from the upper right menu select "Redeem Invitations" and enter the following key:
          |
          |${invitation.token}
          |
          |If you have any trouble or questions please submit a Gemini help desk ticket: https://www.gemini.edu/observing/helpdesk/submit-general-helpdesk-request""".stripMargin
        )

        // A different message could be sent for html clients
        Services.asSuperUser:
          emailService
            .send(pid, emailConfig.invitationFrom, input.recipientEmail, subject, textMessage, none)
      }

      def updateEmailId(invitationId: UserInvitation.Id, emailId: EmailId): F[Result[Unit]] =
        session
          .prepareR(Statements.updateEmailId)
          .use: pq =>
            pq.unique(emailId, invitationId).as(Result.unit)

      @annotation.nowarn("msg=unused implicit parameter")
      def createInvitationAsPi(
        input: CreateUserInvitationInput,
        pid:   Program.Id
      )(using Transaction[F]): F[Result[UserInvitation]] =
        session
          .prepareR(Statements.createInvitationAsPi)
          .use: pq =>
            pq.option(user, pid, input)
              .map(Result.fromOption(_, OdbError.InvalidProgram(pid, "Specified program does not exist, or user is not the PI or COI.".some).asProblem))
              .recoverWith:
                case SqlState.UniqueViolation(ex) =>
                  OdbError.UpdateFailed(s"There is already a pending invitation for program user '${input.programUserId}'.".some).asFailureF

      @annotation.nowarn("msg=unused implicit parameter")
      def createInvitationAsSuperUser(
        input: CreateUserInvitationInput,
        pid:   Program.Id
      )(using Transaction[F]): F[Result[UserInvitation]] =
        session
          .prepareR(Statements.createInvitationAsSuperUser)
          .use: pq =>
            pq.option(user, pid, input)
              .map(Result.fromOption(_, OdbError.InvalidProgram(pid, "Specified program does not exist.".some).asProblem))
              .recoverWith:
                case SqlState.UniqueViolation(ex) =>
                  OdbError.UpdateFailed(s"There is already a pending invitation for program user '${input.programUserId}'.".some).asFailureF

      def createUserInvitationImpl(
        input:        CreateUserInvitationInput,
        targetPid:    Program.Id,
        targetRole:   ProgramUserRole
      )(using Transaction[F]): F[Result[UserInvitation]] =
        user.role match

          // Guest can't create invitations
          case GuestRole             => OdbError.NotAuthorized(user.id, "Guest users cannot create invitations.".some).asFailureF

          // Superusers can do anything
          case ServiceRole(_)        => createInvitationAsSuperUser(input, targetPid)
          case StandardRole.Staff(_) => createInvitationAsSuperUser(input, targetPid)
          case StandardRole.Admin(_) => createInvitationAsSuperUser(input, targetPid)

          // NGO user can't create invitations
          case StandardRole.Ngo(_, p) =>
            OdbError.NotAuthorized(user.id, "NGO users can't create invitations.".some).asFailureF

          // Science users can only create CoI or Observer invitations, and only if they're the PI
          case StandardRole.Pi(_)     =>
            targetRole match
              case ProgramUserRole.Coi      |
                   ProgramUserRole.CoiRO    |
                   ProgramUserRole.External => createInvitationAsPi(input, targetPid)
              case _                        => OdbError.NotAuthorized(user.id, "Science users can only create co-investigator and external (data only) invitations.".some).asFailureF

      def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        (for {
          (p, r, u)  <- ResultT(programUserService.selectLinkData(input.programUserId))
          _          <- u.fold(ResultT.unit[F])(uid => ResultT.fromResult(OdbError.InvalidProgramUser(input.programUserId, s"ProgramUser ${input.programUserId} is already associated with user $uid.".some).asFailure))
          invitation <- ResultT(createUserInvitationImpl(input, p, r))
          emailId    <- ResultT(sendInvitation(input, invitation, p, r))
          _          <- ResultT(updateEmailId(invitation.id, emailId))
        } yield invitation).value

      def redeemUserInvitation(input: RedeemUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]] =
        user match
          case GuestUser(_)                      => OdbError.NotAuthorized(user.id, Some("Guest users cannot redeem user invitations.")).asFailureF
          case ServiceUser(_, _)                 => OdbError.NotAuthorized(user.id, Some("Service users cannot redeem user invitations.")).asFailureF
          case StandardUser(_, _, _, c_duration) =>
            val status = if input.accept then InvitationStatus.Redeemed else InvitationStatus.Declined
            session
              .prepareR(Statements.redeemUserInvitation)
              .use(_.option(user, status, input.key))
              .flatMap:
                case None      =>
                  OdbError.InvitationError(input.key.id, "Invitation is invalid, or has already been accepted, declined, or revoked.".some).asFailureF
                case Some(mid) =>
                  if !input.accept then input.key.id.success.pure
                  else
                    // Here we need to update t_program_user to assign the user id
                    val xa = transaction
                    xa.savepoint.flatMap: sp =>
                      services
                        .programUserService
                        .linkInvitationAccept(mid)
                        .flatMap:
                          case Result.Success(_) => input.key.id.success.pure
                          case f                 => xa.rollback(sp).as(f.as(input.key.id))

      def revokeUserInvitation(input: RevokeUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]] =
        user.role.access match
          case Access.Guest => OdbError.NotAuthorized(user.id, Some("Guest users cannot revoke invitations.")).asFailureF
          case Access.Admin | Access.Service | Access.Staff =>
            session.prepareR(Statements.revokeUserInvitationUnconditionially).use: pq =>
              pq.option(input.id).map: op =>
                Result.fromOption(op, OdbError.InvitationError(input.id, Some(s"Invitation does not exist or is no longer pending.")).asProblem)
          case Access.Ngo | Access.Pi =>
            session.prepareR(Statements.revokeUserInvitation).use: pq =>
              pq.option(input.id, user.id).map: op =>
                Result.fromOption(op, OdbError.InvitationError(input.id, Some(s"Invitation does not exist, is no longer pending, or was issued by someone else.")).asProblem)

  object Statements:

    val createInvitationAsSuperUser: Query[(User, Program.Id, CreateUserInvitationInput), UserInvitation] =
      sql"""
        select insert_invitation(
          $program_user_id,
          $user_id,
          $program_id,
          $email_address,
          null
        ) from t_program
        where c_program_id = $program_id
      """
        .query(user_invitation)
        .contramap((u, pid, input) => (input.programUserId, u.id, pid, input.recipientEmail, pid))

    val createInvitationAsPi: Query[(User, Program.Id, CreateUserInvitationInput), UserInvitation] =
      sql"""
        select insert_invitation(
          $program_user_id,
          $user_id,
          $program_id,
          $email_address,
          null
        ) from t_program
        where c_program_id = $program_id
        and EXISTS (
          SELECT 1
          FROM t_program_user u
          WHERE u.c_program_id = $program_id
          AND   u.c_user_id    = $user_id
          AND   (u.c_role = 'pi' OR u.c_role = 'coi')
        )
      """
        .query(user_invitation)
        .contramap((u, pid, input) => (input.programUserId, u.id, pid, input.recipientEmail, pid, pid, u.id))

    val redeemUserInvitation: Query[(User, InvitationStatus, UserInvitation), ProgramUser.Id] =
      sql"""
        update t_invitation
        set c_status = $user_invitation_status
        where c_status = 'pending'
        and c_invitation_id = $user_invitation_id
        and c_key_hash = md5($varchar)
        and c_issuer_id <> $user_id -- can't redeem your own invitation
        returning c_program_user_id
      """.query(program_user_id)
        .contramap((u, s, i) => (s, i.id, i.body, u.id))

    val revokeUserInvitation: Query[(UserInvitation.Id, User.Id), UserInvitation.Id] =
      sql"""
        update t_invitation
        set c_status = 'revoked'
        where c_invitation_id = $user_invitation_id
        and c_status = 'pending'
        and c_issuer_id = $user_id
        returning c_invitation_id
      """.query(user_invitation_id)

    val revokeUserInvitationUnconditionially: Query[UserInvitation.Id, UserInvitation.Id] =
      sql"""
        update t_invitation
        set c_status = 'revoked'
        where c_invitation_id = $user_invitation_id
        and c_status = 'pending'
        returning c_invitation_id
      """.query(user_invitation_id)

    val updateEmailId: Query[(EmailId, UserInvitation.Id), UserInvitation.Id] =
      sql"""
        update t_invitation
        set c_email_id = $email_id
        where c_invitation_id = $user_invitation_id
        returning c_invitation_id
      """.query(user_invitation_id)
