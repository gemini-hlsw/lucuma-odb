// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.data.UserInvitation
import skunk.Transaction
import Services.Syntax.*
import skunk.syntax.all.*
import lucuma.core.model.User
import skunk.Query
import cats.effect.MonadCancelThrow
import lucuma.odb.util.Codecs.*
import cats.syntax.all.*
import grackle.Result

trait UserInvitationService[F[_]]:

  /** Create an invitation to join a program in a specified role. This current user must be the program's PI. */
  def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]]


object UserInvitationService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): UserInvitationService[F] =
    new UserInvitationService[F]:
      def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        session.prepareR(Statements.CreateUserInvitation).use: ps =>
          ps.option(user, input).flatMap:
            case Some(inv) => Result(inv).pure[F]
            case None      => transaction.rollback.as(Result.failure(s"Specified program does not exist or user ${user.id} is not the PI."))

  object Statements:

    val CreateUserInvitation: Query[(User, CreateUserInvitationInput), UserInvitation] =
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $program_user_role,
          ${program_user_support_type.opt},
          ${tag.opt}
        ) from t_program
        where c_program_id = $program_id
        and   c_pi_user_id = $user_id
      """
        .query(user_invitation)
        .contramap:
          case (u, i) => (u.id, i.programId, i.role, i.supportType, i.supportPartner, i.programId, u.id)
