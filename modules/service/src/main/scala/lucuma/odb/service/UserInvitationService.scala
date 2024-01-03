// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.data.UserInvitation
import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.util.Codecs.*
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

import Services.Syntax.*

trait UserInvitationService[F[_]]:

  /** Create an invitation to join a program in a specified role. This current user must be the program's PI. */
  def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]]


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
