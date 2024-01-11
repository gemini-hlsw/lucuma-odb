// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.data.UserInvitation
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

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): UserInvitationService[F] =
    new UserInvitationService[F]:
      
      def createPiInvitation(pid: Program.Id, role: ProgramUserRole.Coi.type | ProgramUserRole.Observer.type): F[Result[UserInvitation]] =
        session
          .prepareR(Statements.createPiInvitation)
          .use: pq =>
            pq.option(user, pid, role)
              .map(Result.fromOption(_, "Specified program does not exist, or user is not the PI."))

      def createSuperUserInvitation(input: CreateUserInvitationInput) =
        session
          .prepareR(Statements.createSuperUserInvitation)
          .use: pq =>
            pq.option(user, input)
              .map(Result.fromOption(_, "Specified program does not exist."))

      def createNgoInvitation(pid: Program.Id, partner: Tag): F[Result[UserInvitation]] =
        session
          .prepareR(Statements.createNgoInvitation)
          .use: pq =>
            pq.option(user, pid, partner)
              .map(Result.fromOption(_, "Specified program does not exist, or has no partner-allocated time."))

      def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        user.role match

          // Guest can't create invitations
          case GuestRole             => Result.failure("Guest users cannot create invitations.").pure[F]      

          // Superusers can do anything
          case ServiceRole(_)        => createSuperUserInvitation(input)
          case StandardRole.Staff(_) => createSuperUserInvitation(input)
          case StandardRole.Admin(_) => createSuperUserInvitation(input)  

          // NGO user can only create NGO support invitations
          case StandardRole.Ngo(_, p) =>
            input match
              case CreateUserInvitationInput.NgoSupportSupport(pid, t) if t.value == p.tag => createNgoInvitation(pid, t)
              case _ => Result.failure("NGO users can only ngo support invitations, and only for their partner.").pure[F]          

          // Science users can only create CoI or Observer invitations, and only if they're the PI
          case StandardRole.Pi(_)     => 
            input match
              case CreateUserInvitationInput.Coi(pid)      => createPiInvitation(pid, ProgramUserRole.Coi)
              case CreateUserInvitationInput.Observer(pid) => createPiInvitation(pid, ProgramUserRole.Observer)
              case _                                       => Result.failure("Science users can only create co-investigator and observer invitations.").pure[F]

      def redeemUserInvitation(input: RedeemUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]] =
        user match
          case GuestUser(_)                      => Result.failure("Guest users cannot redeemed user invitations.").pure[F]
          case ServiceUser(_, _)                 => Result.failure("Service users cannot redeemed user invitations.").pure[F]
          case StandardUser(_, _, _, c_duration) =>                  
            val status = if input.accept then UserInvitation.Status.Redeemed else UserInvitation.Status.Declined
            session
              .prepareR(Statements.redeemUserInvitation)
              .use(_.option(user, status, input.key))
              .flatMap:
                case None => Result.failure("Invitation is invalid, or has already been accepted, declined, or revoked.").pure[F]
                case Some(r, ot, op, pid) =>
                  val xa = transaction
                  xa.savepoint.flatMap: sp =>
                    session
                      .prepareR(ProgramService.Statements.LinkUser.command)
                      .use(_.execute(pid, user.id, r, ot, op))
                      .as(Result(input.key.id))
                      .recoverWith:
                        case SqlState.UniqueViolation(_) => 
                          xa.rollback(sp).as:
                            Result.warning("You are already in the specified role; no action taken.", input.key.id)
                    
      def revokeUserInvitation(input: RevokeUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation.Id]] =
        user.role.access match
          case Access.Guest => Result.failure("Guest users cannot revoke invitations.").pure[F]
          case Access.Admin | Access.Service | Access.Staff =>
            session.prepareR(Statements.revokeUserInvitationUnconditionially).use: pq =>
              pq.option(input.id).map:
                case Some(id) => Result(id)
                case None     => Result.failure(s"Invitation does not exist or is no longer pending.")
          case Access.Ngo | Access.Pi =>
            session.prepareR(Statements.revokeUserInvitation).use: pq =>
              pq.option(input.id, user.id).map:
                case Some(id) => Result(id)
                case None     => Result.failure(s"Invitation does not exist, is no longer pending, or was issued by someone else.")

  object Statements:

    val createSuperUserInvitation: Query[(User, CreateUserInvitationInput), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $program_user_role,
          ${program_user_support_type.opt},
          ${tag.opt}
        ) from t_program
        where c_program_id = $program_id
      """
        .query(user_invitation)
        .contramap {
          case (u, CreateUserInvitationInput.Coi(pid))                  => (u.id, pid, ProgramUserRole.Coi, None, None, pid)
          case (u, CreateUserInvitationInput.Observer(pid))             => (u.id, pid, ProgramUserRole.Observer, None, None, pid)
          case (u, CreateUserInvitationInput.NgoSupportSupport(pid, p)) => (u.id, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(p), pid)
          case (u, CreateUserInvitationInput.StaffSupport(pid))         => (u.id, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Staff), None, pid)
        }

    val createPiInvitation: Query[(User, Program.Id, ProgramUserRole), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $program_user_role,
          null,
          null
        ) from t_program
        where c_program_id = $program_id
        and c_pi_user_id = $user_id
      """
        .query(user_invitation)
        .contramap((u, pid, r) => (u.id, pid, r, pid, u.id))

    val createNgoInvitation: Query[(User, Program.Id, Tag), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $program_user_role,
          $program_user_support_type,
          $tag
        ) from t_program
        where c_program_id = $program_id
        and exists (select * from t_allocation where c_partner = $tag and c_program_id = $program_id and c_duration > '0'::interval)
      """
        .query(user_invitation)
        .contramap((u, pid, p) => (u.id, pid, ProgramUserRole.Support, ProgramUserSupportType.Partner, p, pid, p, pid))

    val redeemUserInvitation: Query[(User, UserInvitation.Status, UserInvitation), (ProgramUserRole, Option[ProgramUserSupportType], Option[Tag], Program.Id)] =
      sql"""
        update t_invitation
        set c_status = $user_invitation_status, c_redeemer_id = $user_id
        where c_status = 'pending'
        and c_invitation_id = $user_invitation_id
        and c_key_hash = md5($varchar)
        and c_issuer_id <> $user_id -- can't redeem your own invitation
        returning c_role, c_support_type, c_support_partner, c_program_id
      """.query(program_user_role *: program_user_support_type.opt *: tag.opt *: program_id)
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
