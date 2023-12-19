// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.odb.graphql.input.CreateUserInvitationInput
import lucuma.odb.data.UserInvitation
import skunk.Transaction
import Services.Syntax.*
import skunk.syntax.all.*
import lucuma.core.model.User
import cats.effect.MonadCancelThrow
import lucuma.odb.util.Codecs.*
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Staff
import lucuma.core.model.StandardRole.Admin
import lucuma.odb.data.Tag
import skunk.AppliedFragment

trait UserInvitationService[F[_]]:

  /** Create an invitation to join a program in a specified role. This current user must be the program's PI. */
  def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]]


object UserInvitationService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): UserInvitationService[F] =
    new UserInvitationService[F]:
      def createUserInvitation(input: CreateUserInvitationInput)(using Transaction[F]): F[Result[UserInvitation]] =
        val f = Statements.createUserInvitation(user, input)
        session.prepareR(f.fragment.query(user_invitation)).use: ps =>
          ps.option(f.argument).flatMap:
            case Some(inv) => Result(inv).pure[F]
            case None      => transaction.rollback.as(Result.failure(s"Specified program does not exist or user ${user.id} is not the PI."))

  object Statements:

    def createUserInvitation(u: User, i: CreateUserInvitationInput): AppliedFragment =    

      val where: Option[AppliedFragment] = 
        u match
          case GuestUser(_)                => Some(void"false")
          case ServiceUser(_, _)           => None
          case StandardUser(_, role, _, _) =>
            role match
              case Pi(_)           => Some(sql"c_pi_user_id = $user_id"(u.id))
              case Ngo(_, partner) => Some(sql"exists (select * from ... where $tag"(Tag(partner.tag)))
              case Staff(_)        => None
              case Admin(_)        => None              
        
      sql"""
        select insert_invitation(
          $user_id,
          $program_id,
          $program_user_role,
          ${program_user_support_type.opt},
          ${tag.opt}
        ) from t_program
        where c_program_id = $program_id
      """.apply(u.id, i.programId, i.role, i.supportType, i.supportPartner, i.programId)
        |+| where.foldMap(void"and " |+| _)
