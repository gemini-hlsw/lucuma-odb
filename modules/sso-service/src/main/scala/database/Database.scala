// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.database

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.model.*
import lucuma.core.util.Gid
import lucuma.sso.client.ApiKey
import lucuma.sso.service.*
import lucuma.sso.service.orcid.OrcidAccess
import lucuma.sso.service.orcid.OrcidPerson
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.data.Completion
import skunk.data.Completion.Delete
import skunk.implicits.*

// Minimal operations to support the basic use cases … add more when we add the GraphQL interface
trait Database[F[_]] {

  def canonicalizeServiceUser(serviceName: String): F[ServiceUser]

  def createGuestUser: F[GuestUser]
  def createGuestUserSessionToken(guestUser: GuestUser): F[SessionToken]
  def createGuestUserAndSessionToken: F[(GuestUser, SessionToken)]

  def createStandardUserSessionToken(roleId: StandardRole.Id): F[SessionToken]

  def findGuestUserFromToken(token: SessionToken): F[Option[GuestUser]]

  def getGuestUserFromToken(token: SessionToken): F[GuestUser]


  def getStandardUserFromToken(token: SessionToken): F[StandardUser]

  def findStandardUserFromToken(token: SessionToken): F[Option[StandardUser]]

  def findUserFromToken(token: SessionToken): F[Option[User]]

  def getUserFromToken(token: SessionToken): F[User]

  def canonicalizeUser(
    access:    OrcidAccess,
    person:    OrcidPerson,
    role:      RoleRequest
  ) : F[SessionToken]

  /** Create (if necessary) and return the specified role. */
  def canonicalizeRole(user: StandardUser, role: RoleRequest): F[StandardRole.Id]

  /** Create (if necessary) and return the specified role. Fails if `id` is not a *standard* user id. */
  def canonicalizeRole(id: User.Id, role: RoleRequest): F[StandardRole.Id]

  def promoteGuestUser(
    access:    OrcidAccess,
    person:    OrcidPerson,
    gid:       User.Id,
    role:      RoleRequest
  ) : F[(Option[User.Id], SessionToken)]

  def deleteUser(id: User.Id): F[Boolean]
  // // Read
  // def readGuestUser(id: User.Id): F[GuestUser]
  // def readUser(orcid: Orcid): F[StandardUser]

  // // Promote
  // def promoteUser(id: User.Id, profile: OrcidProfile, role: RoleRequest): F[StandardUser]

  def createApiKey(roleId: StandardRole.Id): F[ApiKey]
  def findStandardUserFromApiKey(apiKey: ApiKey): F[Option[StandardUser]]

  /**
   * Delete the specified API key, optionally ensuring that it is owned by the specified user.
   * Returns `true` if the API key was deleted, `false` if no such key exists.
   */
  def deleteApiKey(keyId: PosLong, userId: Option[User.Id]): F[Boolean]
}

object Database extends Codecs {

  def fromSession[F[_]: Concurrent: Trace](s: Session[F]): Database[F] =
    new Database[F] {

      def canonicalizeServiceUser(serviceName: String): F[ServiceUser] =
        Trace[F].span("canonicalizeServiceUser") {
          s.prepareR(CanonicalizeServiceUser).use(_.unique(serviceName))
        }

      def createApiKey(roleId: StandardRole.Id): F[ApiKey] =
        Trace[F].span("createApiKey") {
          s.prepareR(CreateApiKey).use(_.unique(roleId))
        }

      def deleteApiKey(keyId: PosLong, userId: Option[User.Id]): F[Boolean] =
        Trace[F].span("deleteApiKey") {
          Trace[F].put("keyId" -> keyId.toString) *> {
            userId match {
              case Some(u) =>
                Trace[F].put("userId" -> Gid[User.Id].fromString.reverseGet(u)) *>
                s.prepareR(DeleteApiKeyForUser).use(_.execute(keyId, u))
              case None =>
                s.prepareR(DeleteApiKey).use(_.execute(keyId))
            }
          } map {
            case Completion.Delete(1) => true
            case _                    => false
          }
        }

      def createGuestUser: F[GuestUser] =
        Trace[F].span("createGuestUser") {
          s.unique(InsertGuestUser)
        }

      def createGuestUserSessionToken(guestUser: GuestUser): F[SessionToken] =
        Trace[F].span("createGuestUserSessionToken") {
          s.prepareR(InsertGuestUserSessionToken).use(_.unique(guestUser.id))
        }

      def createGuestUserAndSessionToken: F[(GuestUser, SessionToken)] =
        Trace[F].span("createGuestUserAndSessionToken") {
          s.transaction.use { _ =>
            createGuestUser.mproduct(createGuestUserSessionToken) // 🔥
          }
        }

      def createStandardUserSessionToken(roleId: StandardRole.Id): F[SessionToken] =
        Trace[F].span("createStandardUserSessionToken") {
          s.prepareR(InsertStandardUserSessionToken).use(_.unique(roleId))
        }

      def findGuestUserFromToken(token: SessionToken): F[Option[GuestUser]] =
        Trace[F].span("findGuestUserFromToken") {
          s.prepareR(SelectGuestUserForSessionToken).use(_.option(token))
        }

      def getGuestUserFromToken(token: SessionToken): F[GuestUser] =
        Trace[F].span("getGuestUserFromToken") {
          findGuestUserFromToken(token)
            .flatMap(_.toRight(new RuntimeException(s"No guest user for session token: ${token.value}")).liftTo[F])
        }

      def findUserFromToken(token: SessionToken): F[Option[User]] =
        Trace[F].span("findUserFromToken") {
          s.transaction.use { _ =>
            OptionT(findStandardUserFromToken(token).widen[Option[User]])
              .orElse(OptionT(findGuestUserFromToken(token).widen[Option[User]]))
              .value
          }
        }

      def getUserFromToken(token: SessionToken): F[User] =
        Trace[F].span("getUserFromToken") {
          findUserFromToken(token)
            .flatMap(_.toRight(new RuntimeException(s"No user for session token: ${token.value}")).liftTo[F])
        }

      def promoteGuestUser(
        access:    OrcidAccess,
        person:    OrcidPerson,
        gid:       User.Id,
        role:      RoleRequest
      ) : F[(Option[User.Id], SessionToken)] =
        Trace[F].span("promoteGuestUser") {
          s.transaction.use { _ =>

            // Try to update the user profile.
            updateProfile(access, person).flatMap {

              // In this case the user has logged in as someone who already exists in the database.
              case Some(existingUserId) =>
                for {
                  rid <- canonicalizeRoleImpl(existingUserId, role) // find or create requested role
                  tok <- createStandardUserSessionToken(rid)    // create a session token
                  _   <- deleteUser(gid)                        // guest account is no longer needed (session will cascade-delete)
                } yield (Some(existingUserId), tok)             // include the existing user's id since we need to chown the guest's old stuff

              // Promote the guest user.
              case None =>
                for {
                  _   <- deleteAllSessionTokensForUser(gid)      // delete old session
                  rid <- promoteGuest(access, person, gid, role) // promote the guest user
                  tok <- createStandardUserSessionToken(rid)     // create a new one
                } yield (None, tok)                              // no need to chown, user id hasn't changed

            }

          }
        }

      def deleteAllSessionTokensForUser(uid: User.Id): F[Unit] =
        Trace[F].span("deleteAllSessionTokensForUser") {
          s.prepareR(sql"DELETE FROM lucuma_session WHERE user_id = $user_id".command)
            .use(_.execute(uid))
            .void
        }

      def canonicalizeUser(
        access:    OrcidAccess,
        person:    OrcidPerson,
        role:      RoleRequest
      ) : F[SessionToken] =
        Trace[F].span("canonicalizeUser") {
          s.transaction.use { _ =>

            // See if we can update the ORCID profile. If we can then it means it already exists.
            updateProfile(access, person).flatMap {

              // In this case the user has logged in as someone who already exists in the database.
              case Some(existingUserId) =>
                for {
                  rid <- canonicalizeRoleImpl(existingUserId, role) // find or create requested role
                  tok <- createStandardUserSessionToken(rid)    // create a session token
                } yield tok     // return the existing user's id if we're promiting a guest user, plus the token

              // Promote the guest user.
              case None =>
                for {
                  rid <- createStandardUser(access, person, role) // promote the guest user
                  tok <- createStandardUserSessionToken(rid)     // create a session token
                } yield tok

            }
          }

        }

      def canonicalizeRole(user: StandardUser, role: RoleRequest): F[StandardRole.Id] =
        s.transaction.use(_ => canonicalizeRoleImpl(user.id, role))

      def canonicalizeRole(id: User.Id, role: RoleRequest): F[StandardRole.Id] =
        s.transaction.use(_ => canonicalizeRoleImpl(id, role))
        
      private def canonicalizeRoleImpl(userId: User.Id, role: RoleRequest): F[StandardRole.Id] =
        Trace[F].span("canonicalizeRoleImpl") {
          // we assume we're in a transction … would be nice if we could put this in the type
          OptionT(findRole(userId, role)).getOrElseF(addRole(userId, role))
        }

      def findRole(userId: User.Id, role: RoleRequest): F[Option[StandardRole.Id]] =
        Trace[F].span("findRole") {

          // Query depends on whether there's a partner or not.
          val af: AppliedFragment =
            sql"""
              SELECT role_id
              FROM   lucuma_role
              WHERE  user_id = $user_id
              AND    role_type = $role_type
            """.apply(userId, role.tpe) |+|
            role.partnerOption.fold(AppliedFragment.empty)(sql" AND role_ngo = $partner")

          // Done
          s.prepareR(af.fragment.query(role_id)).use(pq => pq.option(af.argument))

        }

      // def getDefaultRole(id: User.Id): F[Option[StandardRole.Id]] =
      //   s.prepareR(SelectDefaultRole).use(_.option(id))

      def deleteUser(id: User.Id): F[Boolean] =
        Trace[F].span("deleteUser") {
          s.prepareR(DeleteUser).use { pq =>
            pq.execute(id).map {
              case Delete(c) => c > 0
              case _         => sys.error("unpossible")
            }
          }
        }

      /// HELPERS

      // Update the specified ORCID profile and yield the associated `StandardUser`, if any.
      def updateProfile(access: OrcidAccess, person: OrcidPerson): F[Option[User.Id]] =
        Trace[F].span("updateProfile"):
          s.prepareR(UpdateProfile).use(_.option(access, person))

      def promoteGuest(
        access:    OrcidAccess,
        person:    OrcidPerson,
        gid:       User.Id,
        role:      RoleRequest
      ): F[StandardRole.Id] =
        Trace[F].span("promoteGuest") {
          s.prepareR(PromoteGuest).use { pq =>
            for {
              userId <- pq.unique(gid, access, person)
              rid    <- addRole(userId, role)
            } yield rid
          }
        }

      def createStandardUser(
        access:    OrcidAccess,
        person:    OrcidPerson,
        role:      RoleRequest
      ): F[StandardRole.Id] =
        Trace[F].span("createStandardUser") {
          s.prepareR(InsertStandardUser).use { pq =>
            for {
              userId <- pq.unique(access, person)
              rid    <- addRole(userId, role)
            } yield rid
          }
        }

      def addRole(user: User.Id, role: RoleRequest): F[StandardRole.Id] =
        Trace[F].span("addRole") {
          s.prepareR(InsertRole).use(_.unique(user, role))
        }

      def getStandardUserFromToken(token: SessionToken): F[StandardUser] =
        Trace[F].span("getStandardUserFromToken") {
          findStandardUserFromToken(token).flatMap {
            case None => Concurrent[F].raiseError(new RuntimeException(s"No such standard user with session token: ${token.value}"))
            case Some(u) => u.pure[F]
          }
        }

      def findStandardUserFromToken(
        token: SessionToken
      ): F[Option[StandardUser]] =
        Trace[F].span("findStandardUserFromToken") {
          s.prepareR(SelectStandardUserByToken).use { pq =>
            pq.stream(token, 64).compile.toList flatMap {
              case Nil => none[StandardUser].pure[F]
              case all @ ((roleId, user, _) :: _) =>
                val roles = all.map(_._3)
                roles.find(_.id === roleId) match {
                  case None => Concurrent[F].raiseError(new RuntimeException(s"Unpossible: active role was not found for standard user token: ${token.value}"))
                  case Some(activeRole) =>
                    (user.copy(role = activeRole, otherRoles = roles.filterNot(_.id === roleId))).some.pure[F]
                }
            }
          }
        }

      def findStandardUserFromApiKey(
        apiKey: ApiKey
      ): F[Option[StandardUser]] =
        Trace[F].span("findStandardUserFromApiKey") {
          s.prepareR(SelectStandardUserByApiKey).use { pq =>
            pq.stream(apiKey, 64).compile.toList flatMap {
              case Nil => none[StandardUser].pure[F]
              case all @ ((roleId, user, _) :: _) =>
                val roles = all.map(_._3)
                roles.find(_.id === roleId) match {
                  case None => Concurrent[F].raiseError(new RuntimeException(s"Unpossible: active role was not found for standard user associated with API key: ${apiKey.id}"))
                  case Some(activeRole) =>
                    (user.copy(role = activeRole, otherRoles = roles.filterNot(_.id === roleId))).some.pure[F]
                }
            }
          }
        }
    }

  private val InsertGuestUser: Query[Void, GuestUser] =
    sql"""
      INSERT INTO lucuma_user (user_type)
      VALUES ('guest')
      RETURNING user_id
    """.query(user_id.map(GuestUser(_)))

  private val UpdateProfile: Query[(OrcidAccess, OrcidPerson), User.Id] =
    sql"""
      UPDATE lucuma_user
      SET orcid_access_token     = $uuid,
       -- orcid_token_expiration -- TODO
          orcid_given_name       = ${varchar.opt},
          orcid_credit_name      = ${varchar.opt},
          orcid_family_name      = ${varchar.opt},
          orcid_email            = ${varchar.opt}
      WHERE orcid_id = $orcid_id
      RETURNING (user_id)
    """
      .query(user_id)
      .contramap[(OrcidAccess, OrcidPerson)] {
        case (access, person) =>
          access.accessToken               *: // TODO: expiration
          person.name.givenName            *:
          person.name.creditName           *:
          person.name.familyName           *:
          person.primaryEmail.map(_.email) *:
          access.orcidId                   *: EmptyTuple
      }

  private val PromoteGuest: Query[(User.Id, OrcidAccess, OrcidPerson), User.Id] =
    sql"""
      UPDATE lucuma_user
      SET user_type              = 'standard',
          orcid_id               = $orcid_id,
          orcid_access_token     = $uuid,
       -- orcid_token_expiration -- TODO
          orcid_given_name       = ${varchar.opt},
          orcid_credit_name      = ${varchar.opt},
          orcid_family_name      = ${varchar.opt},
          orcid_email            = ${varchar.opt}
      WHERE user_id   = $user_id
      AND   user_type = 'guest' -- sanity check
      RETURNING (user_id)
    """
      .query(user_id)
      .contramap[(User.Id, OrcidAccess, OrcidPerson)] {
        case (id, access, person) =>
          access.orcidId                   *:
          access.accessToken               *: // TODO: expiration
          person.name.givenName            *:
          person.name.creditName           *:
          person.name.familyName           *:
          person.primaryEmail.map(_.email) *:
          id                               *: EmptyTuple
      }

  private val InsertStandardUser: Query[(OrcidAccess, OrcidPerson), User.Id] =
    sql"""
      INSERT INTO lucuma_user (
        user_type,
        orcid_id,
        orcid_access_token,
     -- orcid_token_expiration, -- TODO
        orcid_given_name,
        orcid_credit_name,
        orcid_family_name,
        orcid_email
      )
      VALUES (
        'standard',
        $orcid_id,
        $uuid,
        ${varchar.opt},
        ${varchar.opt},
        ${varchar.opt},
        ${varchar.opt}
      )
      RETURNING (user_id)
    """
      .query(user_id)
      .contramap[(OrcidAccess, OrcidPerson)] {
        case (access, person) =>
          access.orcidId                   *:
          access.accessToken               *: // TODO: expiration
          person.name.givenName            *:
          person.name.creditName           *:
          person.name.familyName           *:
          person.primaryEmail.map(_.email) *: EmptyTuple
      }

  /**
   * Query that reads a single standard user as a list of triples. The first two values are
   * constant (active role id, user with null role and Nil otherRoles) and the last value is a unique
   * role, from which the active and other roles must be selected.
   */
  private val SelectStandardUserByToken: Query[SessionToken, (StandardRole.Id, StandardUser, StandardRole)] =
    sql"""
      SELECT
        s.role_id,
        u.user_id,
        u.orcid_id,
        u.orcid_given_name,
        u.orcid_credit_name,
        u.orcid_family_name,
        u.orcid_email,
        r.role_id,
        r.role_type,
        r.role_ngo
      FROM lucuma_session s
      JOIN lucuma_user    u on u.user_id = s.user_id
      JOIN lucuma_role    r on r.user_id = s.user_id
      WHERE s.refresh_token = $session_token
      AND   s.user_type     = 'standard'
    """.query(
        role_id *:
        (user_id *: orcid_id *: varchar.opt *: varchar.opt *: varchar.opt *: varchar.opt).map {
          case (id, orcidId, givenName, creditName, familyName, email) =>
            StandardUser(
              id         = id,
              role       = null, // TODO
              otherRoles = Nil, // TODO
              profile    = OrcidProfile(
                orcidId  = orcidId,
                profile  = UserProfile(
                  givenName  = givenName,
                  creditName = creditName,
                  familyName = familyName,
                  email      = email
                )
              )
            )
        } *:
        (role_id *: role_type *: partner.opt).map {
          // we really need emap here
          case (id, RoleType.Admin, None)    => StandardRole.Admin(id)
          case (id, RoleType.Staff, None)    => StandardRole.Staff(id)
          case (id, RoleType.Ngo,   Some(p)) => StandardRole.Ngo(id, p)
          case (id, RoleType.Pi,    None)    => StandardRole.Pi(id)
          case hmm => sys.error(s"welp, we really need emap here, sorry. anyway, invalid: $hmm")
        }
      )

  val SelectStandardUserByApiKey: Query[ApiKey, (StandardRole.Id, StandardUser, StandardRole)] =
    sql"""
      SELECT
        k.role_id,
        u.user_id,
        u.orcid_id,
        u.orcid_given_name,
        u.orcid_credit_name,
        u.orcid_family_name,
        u.orcid_email,
        r.role_id,
        r.role_type,
        r.role_ngo
      FROM lucuma_api_key k
      JOIN lucuma_user    u on u.user_id = k.user_id
      JOIN lucuma_role    r on r.user_id = k.user_id
      WHERE k.api_key_id = $varchar
      AND   k.api_key_hash = md5($varchar);
    """
      .contramap[ApiKey](k => (k.id.value.toHexString, k.body))
      .query(
        role_id *:
        (user_id *: orcid_id *: varchar.opt *: varchar.opt *: varchar.opt *: varchar.opt).map {
          case (id, orcidId, givenName, creditName, familyName, email) =>
          StandardUser(
            id         = id,
            role       = null, // NOTE
            otherRoles = Nil,  // NOTE
            profile    = OrcidProfile(
              orcidId  = orcidId,
              profile  = UserProfile(
                givenName  = givenName,
                creditName = creditName,
                familyName = familyName,
                email      = email
              )
            )
          )
        } *:
        (role_id *: role_type *: partner.opt).emap {
          case (id, RoleType.Admin, None   ) => StandardRole.Admin(id).asRight
          case (id, RoleType.Staff, None   ) => StandardRole.Staff(id).asRight
          case (id, RoleType.Ngo  , Some(p)) => StandardRole.Ngo(id, p).asRight
          case (id, RoleType.Pi   , None   ) => StandardRole.Pi(id).asRight
          case hmm                           => s"Invalid: $hmm".asLeft
        }
      )

  private val DeleteUser: Command[User.Id] =
    sql"""
      DELETE FROM lucuma_user WHERE user_id = $user_id
    """.command

  private val InsertRole: Query[(User.Id, RoleRequest), StandardRole.Id] =
    sql"""
      INSERT INTO lucuma_role (user_id, role_type, role_ngo)
      VALUES ($user_id, $role_type, ${partner.opt})
      RETURNING role_id
    """
      .query(role_id)
      .contramap { case (id, rr) => (id, rr.tpe, rr.partnerOption) }


  /** Create a session token for a guest user. */
  private val InsertGuestUserSessionToken: Query[User.Id, SessionToken] =
    sql"""
      INSERT INTO lucuma_session (user_id, user_type, role_id)
      VALUES ($user_id, 'guest', null)
      RETURNING refresh_token
      """.query(session_token)

  /** Create a session token for a standard user [role]. */
  private val InsertStandardUserSessionToken: Query[StandardRole.Id, SessionToken] =
    sql"""
      INSERT INTO lucuma_session (user_id, user_type, role_id)
      SELECT user_id, user_type, role_id
      FROM lucuma_role
      WHERE role_id = $role_id
      AND user_type = 'standard' -- sanity check
      RETURNING refresh_token
      """.query(session_token)

  private val SelectGuestUserForSessionToken: Query[SessionToken, GuestUser] =
    sql"""
      SELECT user_id
      FROM lucuma_session
      WHERE user_type = 'guest'
      AND   refresh_token = $session_token
    """.query(user_id).map(GuestUser(_))

  private val CreateApiKey: Query[StandardRole.Id, ApiKey] =
    sql"""
      SELECT insert_api_key(user_id, role_id)
      FROM   lucuma_role
      WHERE  role_id = $role_id
    """.query(api_key)

  private val DeleteApiKey: Command[PosLong] =
    sql"""
      DELETE FROM lucuma_api_key
      WHERE  api_key_id = $varchar
    """
      .contramap[PosLong](_.value.toHexString)
      .command

  private val DeleteApiKeyForUser: Command[(PosLong, User.Id)] =
    sql"""
      DELETE FROM lucuma_api_key
      WHERE  api_key_id = $varchar
      AND    user_id = $user_id
    """
      .contramap[(PosLong, User.Id)] { case (key, user) => (key.value.toHexString, user) }
      .command

  val CanonicalizeServiceUser: Query[String, ServiceUser] =
    sql"""
      INSERT INTO lucuma_user (user_type, service_name)
      VALUES ('service', $varchar)
      ON CONFLICT (service_name) DO
        UPDATE SET service_name=EXCLUDED.service_name
      RETURNING user_id, service_name
    """
      .query(user_id *: varchar)
      .to[ServiceUser]

}
