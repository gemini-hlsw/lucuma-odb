// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.implicits.*
import lucuma.core.model.*
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.headers.Location

object SetRoleSuite extends SsoSuite with Fixture with FlakyTests {

  def setRole(rid: StandardRole.Id)  = 
    (SsoRoot / "auth" / "v1" / "set-role").withQueryParam("role", rid.toString)

  test("Set role.") {
    SsoSimulator[IO].use { case (db, sim, sso, _, _) =>
      val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
      for {

        // Log in as Bob, who is new.
        redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
        stage2 <- sim.authenticate(redir, Bob, None)
        tok1   <- sso.get(stage2)(CookieReader[IO].getSessionToken)
        user1  <- db.use(_.getStandardUserFromToken(tok1))

        // Add an Admin role for Bob
        rid    <- db.use(_.canonicalizeRole(user1, RoleRequest.Admin))

        // Switch the role
        tok2   <- sso.get(setRole(rid))(CookieReader[IO].getSessionToken)
        user2  <- db.use(_.getStandardUserFromToken(tok2))

      } yield expect(
        user1.role.access === Access.Pi &&
        user2.role.access === Access.Admin &&
        user1.id === user2.id &&
        user2.role.id === rid &&
        user2.otherRoles.contains(user1.role)
      )
    }
  }

}