// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.syntax.all.*
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.ApiKey
import lucuma.sso.service.database.Database
import lucuma.sso.service.simulator.OrcidSimulator
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.Location
import skunk.codec.all.bool
import skunk.implicits.*

class DisabledUserSuite extends SsoSuite with Fixture:

  val Stage1: Uri =
    (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)

  val RefreshToken: Request[IO] =
    Request[IO](Method.POST, SsoRoot / "api" / "v1" / "refresh-token")

  // Nothing in the service changes `user_enabled`; staff do it in SQL.
  def setEnabled(id: User.Id, enabled: Boolean): IO[Unit] =
    FMain.databasePoolResource[IO](SsoSimulator.config.database).flatten.use: session =>
      session
        .prepareR(sql"UPDATE lucuma_user SET user_enabled = $bool WHERE user_id = ${Database.user_id}".command)
        .use(_.execute(enabled, id))
        .void

  def stage2Uri(sso: Client[IO], sim: OrcidSimulator[IO], user: Option[StandardUser]): IO[Uri] =
    sso.get(Stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
      .flatMap(sim.authenticate(_, Bob, user.map(_.profile.orcidId)))

  def loginAsBob(sso: Client[IO], sim: OrcidSimulator[IO], db: Resource[IO, Database[IO]]): IO[StandardUser] =
    for
      stage2 <- stage2Uri(sso, sim, None)
      token  <- sso.get(stage2)(CookieReader[IO].getSessionToken)
      user   <- db.use(_.getStandardUserFromToken(token))
    yield user

  test("A disabled user cannot refresh."):
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      for
        bob     <- loginAsBob(sso, sim, db)
        before  <- sso.status(RefreshToken)
        _       <- setEnabled(bob.id, false)
        after   <- sso.status(RefreshToken)
        // A session created after the disabling must not work either.
        token   <- db.use(_.createStandardUserSessionToken(bob.role.id))
        found   <- db.use(_.findUserFromToken(token))
      yield
        assertEq(Status.Ok, before)
        assertEq(Status.Forbidden, after)
        assertEq(none[User], found)

  test("A disabled guest cannot refresh."):
    SsoSimulator[IO].use: (db, _, sso, _, _) =>
      for
        token  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getSessionToken)
        guest  <- db.use(_.getGuestUserFromToken(token))
        _      <- setEnabled(guest.id, false)
        status <- sso.status(RefreshToken)
        // A session created after the disabling must not work either.
        tokenʹ <- db.use(_.createGuestUserSessionToken(guest))
        found  <- db.use(_.findUserFromToken(tokenʹ))
      yield
        assertEq(Status.Forbidden, status)
        assertEq(none[User], found)

  test("A disabled user cannot exchange an API key, even after being enabled again."):
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      def redeem(key: ApiKey): IO[Option[StandardUser]] =
        db.use(_.findStandardUserFromApiKey(key))
      for
        bob      <- loginAsBob(sso, sim, db)
        oldKey   <- db.use(_.createApiKey(bob.role.id))
        before   <- redeem(oldKey)
        _        <- setEnabled(bob.id, false)
        disabled <- redeem(oldKey)
        // A JWT issued before the disabling can still create a key.
        newKey   <- db.use(_.createApiKey(bob.role.id))
        created  <- redeem(newKey)
        _        <- setEnabled(bob.id, true)
        enabled  <- (redeem(oldKey), redeem(newKey)).tupled
      yield
        assert(before.isDefined)
        assertEq(none[StandardUser], disabled)
        assertEq(none[StandardUser], created)
        assertEq((none[StandardUser], none[StandardUser]), enabled)

  test("A disabled user cannot log in."):
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      for
        bob    <- loginAsBob(sso, sim, db)
        _      <- setEnabled(bob.id, false)
        stage2 <- stage2Uri(sso, sim, bob.some)
        login  <- sso.status(Request[IO](uri = stage2))
        status <- sso.status(RefreshToken)
      yield
        assertEq(Status.Forbidden, login)
        assertEq(Status.Forbidden, status)

  test("A guest who logs in as a disabled user stays a guest."):
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      for
        bob    <- loginAsBob(sso, sim, db)
        _      <- setEnabled(bob.id, false)
        token  <- sso.run(Request(Method.POST, SsoRoot / "api" / "v1" / "auth-as-guest")).use(CookieReader[IO].getSessionToken)
        guest  <- db.use(_.getGuestUserFromToken(token))
        stage2 <- stage2Uri(sso, sim, bob.some)
        login  <- sso.status(Request[IO](uri = stage2))
        found  <- db.use(_.findUserFromToken(token))
      yield
        assertEq(Status.Forbidden, login)
        assertEq(guest.some.widen[User], found)
