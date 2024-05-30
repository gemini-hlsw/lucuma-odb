// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.core.data.EmailAddress
import lucuma.core.enums.InvitationStatus
import lucuma.core.enums.Partner
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
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
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.graphql.input.RedeemUserInvitationInput
import lucuma.odb.graphql.input.RevokeUserInvitationInput
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
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

  // temporary
  extension (self: OdbError.InvitationError.type)
    def apply(id: UserInvitation.Id, detail: Option[String]): OdbError.InvitationError =
      OdbError.InvitationError(UserInvitation.Id.fromString.reverseGet(id), detail)

  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): UserInvitationService[F] =
    new UserInvitationService[F]:
      
      def sendInvitation(input: CreateUserInvitationInput, invitation: UserInvitation)(
        using Transaction[F]): F[Result[EmailId]] = {
        val subject: NonEmptyString = NonEmptyString.unsafeFrom(
          s"Invitation to collaborate from ${user.displayName}"
        )
      
        val textMessage: NonEmptyString = NonEmptyString.unsafeFrom(
          s"""${user.displayName} has invited you to collaborate on a Gemini proposal. To accept this invitation go to ${emailConfig.exploreUrl} to log in, then from the upper right menu select "Redeem Invitations" and enter the following key:
          |
          |${invitation.token}
          |
          |If you have any trouble or questions please submit a Gemini help desk ticket: https://www.gemini.edu/observing/helpdesk/submit-general-helpdesk-request""".stripMargin
        )

        // A different message could be sent for html clients
        emailService(emailConfig, httpClient)
          .send(input.programId, emailConfig.invitationFrom, input.recipientEmail, subject, textMessage, none)
      }

      def updateEmailId(invitationId: UserInvitation.Id, emailId: EmailId): F[Result[Unit]] =
        session
          .prepareR(Statements.updateEmailId)
          .use: pq =>
            pq.unique(emailId, invitationId).as(Result.unit)

      def createInvitationAsPi(pid: Program.Id, email: EmailAddress, role: ProgramUserRole.Coi.type | ProgramUserRole.CoiRO.type, partner: Partner): F[Result[UserInvitation]] =
        session
          .prepareR(Statements.createInvitationAsPi)
          .use: pq =>
            pq.option(user, pid, email, role, partner)
              .map(Result.fromOption(_, OdbError.InvalidProgram(pid, Some("Specified program does not exist, or user is not the PI.")).asProblem))

      def createInvitationAsSuperUser(input: CreateUserInvitationInput) =
        session
          .prepareR(Statements.createInvitationAsSuperUser)
          .use: pq =>
            pq.option(user, input)
              .map(Result.fromOption(_, OdbError.InvalidProgram(input.programId, Some("Specified program does not exist.")).asProblem))

      def createUserInvitationImpl(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        user.role match

          // Guest can't create invitations
          case GuestRole             => OdbError.NotAuthorized(user.id, Some("Guest users cannot create invitations.")).asFailureF

          // Superusers can do anything
          case ServiceRole(_)        => createInvitationAsSuperUser(input)
          case StandardRole.Staff(_) => createInvitationAsSuperUser(input)
          case StandardRole.Admin(_) => createInvitationAsSuperUser(input)

          // NGO user can't create invitations
          case StandardRole.Ngo(_, p) =>
            OdbError.NotAuthorized(user.id, Some("NGO users can't create invitations.")).asFailureF

          // Science users can only create CoI or Observer invitations, and only if they're the PI
          case StandardRole.Pi(_)     =>
            input match
              case CreateUserInvitationInput.Coi(pid, e, partner)   => createInvitationAsPi(pid, e, ProgramUserRole.Coi, partner)
              case CreateUserInvitationInput.CoiRO(pid, e, partner) => createInvitationAsPi(pid, e, ProgramUserRole.CoiRO, partner)
              case _                                          => OdbError.NotAuthorized(user.id, Some("Science users can only create co-investigator and observer invitations.")).asFailureF

      def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        (for {
          invitation <- ResultT(createUserInvitationImpl(input))
          emailId    <- ResultT(sendInvitation(input, invitation))
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
                case None => OdbError.InvitationError(input.key.id, Some("Invitation is invalid, or has already been accepted, declined, or revoked.")).asFailureF
                case Some(r, pid, partner) =>
                  val xa = transaction
                  xa.savepoint.flatMap: sp =>
                    session
                      .prepareR(ProgramService.Statements.LinkUser.command)
                      .use(_.execute(pid, user.id, r, partner))
                      .as(Result(input.key.id))
                      .recoverWith:
                        case SqlState.UniqueViolation(_) =>
                          xa.rollback(sp).as:
                            OdbError.NoAction(Some("You are already in the specified role; no action taken.")).asWarning(input.key.id)

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

    val createInvitationAsSuperUser: Query[(User, CreateUserInvitationInput), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $email_address,
          $program_user_role,
          ${tag.opt},
          null
        ) from t_program
        where c_program_id = $program_id
      """
        .query(user_invitation)
        .contramap {
          case (u, CreateUserInvitationInput.Coi(pid, e, partner)) => (u.id, pid, e, ProgramUserRole.Coi, Tag(partner.tag).some, pid)
          case (u, CreateUserInvitationInput.CoiRO(pid, e, partner)) => (u.id, pid, e, ProgramUserRole.CoiRO, Tag(partner.tag).some, pid)
          case (u, CreateUserInvitationInput.Support(pid, e)) => (u.id, pid, e, ProgramUserRole.Support, none, pid)
        }

    val createInvitationAsPi: Query[(User, Program.Id, EmailAddress, ProgramUserRole, Partner), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $email_address,
          $program_user_role,
          $tag,
          null
        ) from t_program
        where c_program_id = $program_id
        and c_pi_user_id = $user_id
      """
        .query(user_invitation)
        .contramap((u, pid, e, r, p) => (u.id, pid, e, r, Tag(p.tag), pid, u.id))

    val redeemUserInvitation: Query[(User, InvitationStatus, UserInvitation), (ProgramUserRole, Program.Id, Option[Tag])] =
      sql"""
        update t_invitation
        set c_status = $user_invitation_status, c_redeemer_id = $user_id
        where c_status = 'pending'
        and c_invitation_id = $user_invitation_id
        and c_key_hash = md5($varchar)
        and c_issuer_id <> $user_id -- can't redeem your own invitation
        returning c_role, c_program_id, c_partner
      """.query(program_user_role *: program_id *: tag.opt)
        .contramap((u, s, i) => (s, u.id, i.id, i.body, u.id))

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
