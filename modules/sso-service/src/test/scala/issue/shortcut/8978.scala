// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package issue.shortcut


import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.OrcidId
import lucuma.core.model.StandardUser
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.orcid.OrcidIdGenerator
import lucuma.sso.service.orcid.OrcidPerson
import org.http4s.*
import org.http4s.headers.Location
class Shortcut_8978 extends SsoSuite with Fixture with OrcidIdGenerator[IO]:

  def createOrLoginAs(person: OrcidPerson, id: OrcidId): IO[(SessionToken, StandardUser)] =
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
      for {
        res    <- sso.get(stage1)(_.pure[IO])
        _      <- IO(res.status === Status.Found).assert
        loc     = res.headers.get[Location].map(_.uri)
        _      <- IO(loc.isDefined).assert
        stage2 <- sim.authenticate(loc.get, person, Some(id))
        tok    <- sso.get(stage2)(CookieReader[IO].getSessionToken)
        user   <- db.use(_.getStandardUserFromToken(tok))
      } yield (tok, user)

  test("deleteAllSessionTokensForUser"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (tok, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                _ <- db.deleteAllSessionTokensForUser(user.id)
                u <- db.findUserFromToken(tok)
              yield assertEq(u, None)

  test("deleteUser"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (tok, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                _ <- db.deleteUser(user.id)
                u <- db.findUserFromToken(tok)
              yield assertEq(u, None)

  test("deleteAllSessionTokensForRole"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (tok, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                _ <- db.deleteAllSessionTokensForRole(user.role.id)
                u <- db.findUserFromToken(tok)
              yield assertEq(u, None)
  
  test("deleteRole"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (tok, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                _     <- db.deleteRole(user.role.id) // the last role!
                u     <- db.findUserFromToken(tok)
                _     <- IO(u === None).assert
              yield ()

  test("deleteRole; can't delete PI role"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (_, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                _     <- db.deleteRole(user.role.id) // the last role!
                user2 <- createOrLoginAs(Bob, bobId).map(_._2)
                _     <- IO(user === user2).assert // same everything
              yield ()

  test("deleteRole; if it's an unused role then existing tokens should continue to work"):
    randomOrcidId.flatMap: bobId =>
      createOrLoginAs(Bob, bobId).flatMap: (tok, user) =>
        SsoSimulator[IO].map(_._1).use: db =>
            db.use: db =>
              for
                id    <- db.canonicalizeRole(user.id, RoleRequest.Staff)
                user2 <- db.getStandardUserFromToken(tok)
                _     <- IO(user2.otherRoles.exists(_.id === id)).assert // new role should be there
                _     <- db.deleteRole(id)
                user3 <- db.getStandardUserFromToken(tok)
                _     <- IO(user3.otherRoles.isEmpty).assert // new role should not be there
              yield ()