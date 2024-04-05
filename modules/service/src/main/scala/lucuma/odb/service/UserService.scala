// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.Command
import skunk.Session
import skunk.codec.all.*
import skunk.implicits.*

trait UserService[F[_]] {
  def canonicalizeUser(u: User)(using Services.SuperUserAccess): F[Unit]
}

object UserService {

  def fromSession[F[_]: MonadCancelThrow: Trace](s: Session[F]): UserService[F] =
    new UserService[F] {
      import Statements._

      def canonicalizeUser(u: User)(using Services.SuperUserAccess): F[Unit] =
        Trace[F].span("canonicalizeUser") {
          u match {
            case gu @ GuestUser(_)             => canonicalizeGuestUser(gu)
            case su @ ServiceUser(_, _)        => canonicalizeServiceUser(su)
            case su @ StandardUser(_, _, _, _) => canonicalizeStandardUser(su)
          }
        }

      def canonicalizeGuestUser(gu: GuestUser): F[Unit] =
        Trace[F].span("canonicalizeGuestUser") {
          s.prepareR(CanonicalizeGuestUser).use(_.execute(gu)).void
        }

      def canonicalizeServiceUser(su: ServiceUser): F[Unit] =
        Trace[F].span("canonicalizeServiceUser") {
          s.prepareR(CanonicalizeServiceUser).use(_.execute(su)).void
        }

      def canonicalizeStandardUser(su: StandardUser): F[Unit] =
        Trace[F].span("canonicalizeStandardUser") {
          s.prepareR(CanonicalizeStandardUser).use(_.execute(su)).void
        }

    }

  object Statements {

    // Guest users never change.
    val CanonicalizeGuestUser: Command[GuestUser] =
      sql"""
        insert into t_user (c_user_id, c_user_type)
        values ($user_id, 'guest')
        on conflict (c_user_id) do nothing
      """.command.contramap(_.id)

    // Service users should never change but we'll accommodate a name change here.
    val CanonicalizeServiceUser: Command[ServiceUser] =
      sql"""
        insert into t_user (c_user_id, c_user_type, c_service_name)
        values ($user_id, 'service', $varchar)
        on conflict (c_user_id) do
        update set c_service_name = $varchar
      """.command.contramap { su => (su.id, su.name, su.name) }

    // Standard user ORCID profiles can change.
    val CanonicalizeStandardUser: Command[StandardUser] =
      sql"""
        insert into t_user (
          c_user_id,
          c_user_type,
          c_orcid_id,
          c_orcid_given_name,
          c_orcid_credit_name,
          c_orcid_family_name,
          c_orcid_email
        ) values (
          $user_id,
          'standard',
          $orcid_id,
          ${varchar.opt},
          ${varchar.opt},
          ${varchar.opt},
          ${varchar.opt}
        ) on conflict (c_user_id) do update
        set c_orcid_given_name  = ${varchar.opt},
            c_orcid_credit_name = ${varchar.opt},
            c_orcid_family_name = ${varchar.opt},
            c_orcid_email       = ${varchar.opt}
      """.command.contramap { su =>
        val p = su.profile
        (su.id, p.orcidId, p.givenName, p.creditName, p.familyName, p.primaryEmail,
                p.givenName, p.creditName, p.familyName, p.primaryEmail)
      }

  }

}