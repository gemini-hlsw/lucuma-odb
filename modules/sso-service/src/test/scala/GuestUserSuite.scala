// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.syntax.all.*
import lucuma.core.model.GuestRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.SsoJwtClaim
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.*
import org.http4s.headers.Location

object GuestUserSuite extends SsoSuite with Fixture with FlakyTests {

  test("Guest Login.") {
    flaky()(
      SsoSimulator[IO]
        .flatMap { case (pool, _, sso, jwtReader, _) => pool.map(db => (db, sso, jwtReader)) }
        .use { case (db, sso, jwtReader) =>
          sso.run(
            Request(
              method = Method.POST,
              uri    = SsoRoot / "api" / "v1" / "auth-as-guest"
            )
          ).use { res =>
            import jwtReader.entityDecoder // note
            for {
              jwt   <- res.as[SsoJwtClaim]                    // response body is a jwt
              user  <- jwt.getUser.liftTo[IO]                 // get the user
              tok   <- CookieReader[IO].getSessionToken(res)  // get the new session token
              userʹ <- db.getGuestUserFromToken(tok)          // redeem it to get the same user
            } yield
              expect(res.status == Status.Created) &&
              expect(user.role  == GuestRole) &&
              expect(user.id == userʹ.id)
          }
        }
    )
  }

  test("Guest user promotion.") {
    flaky(){
      val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
      SsoSimulator[IO]
        .flatMap { case (pool, sim, sso, jwtReader, _) => pool.map(db => (db, sim, sso, jwtReader)) }
        .use { case (db, sim, sso, jwtReader) =>

          val loginAsGuest: IO[(User.Id, SessionToken)] =
            sso.run(
              Request(
                method = Method.POST,
                uri    = SsoRoot / "api" / "v1" / "auth-as-guest"
              )
            ).use { res =>
              import jwtReader.entityDecoder // note
              for {
                jwt   <- res.as[SsoJwtClaim]
                user  <- jwt.getUser.liftTo[IO]
                tok   <- CookieReader[IO].getSessionToken(res)
              } yield (user.id, tok)
            }

          val loginAsBob: IO[(User.Id, SessionToken)] =
            for {
              redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
              stage2 <- sim.authenticate(redir, Bob, None)
              tok    <- sso.get(stage2)(CookieReader[IO].getSessionToken)
              user   <- db.getUserFromToken(tok)
            } yield (user.id, tok)

          // Ok here we go.
          loginAsGuest.flatMap { case (guestId, guestToken) =>
            loginAsBob.flatMap { case (bobId, bobToken) =>
              db.findUserFromToken(guestToken).map { op =>
                expect(op.isEmpty)             && // old token should no longer work
                expect(guestId == bobId)       && // bob and guest have the same id
                expect(guestToken != bobToken)    // and different tokens
              }
            }
          }
        }
    }
  }

  test("Log in as guest, then as existing user.") {
    flaky()(
      SsoSimulator[IO].use { case (db, sim, sso, _, _) =>
        val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
        for {

          // Log in as Bob, who is new.
          redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
          stage2 <- sim.authenticate(redir, Bob, None)
          tok1   <- sso.get(stage2)(CookieReader[IO].getSessionToken)
          user1  <- db.use(_.getStandardUserFromToken(tok1))

          // Log in as a guest. This will give us a new session cookie.
          gtok   <- sso.run(
                      Request(
                        method = Method.POST,
                        uri    = SsoRoot / "api" / "v1" / "auth-as-guest"
                      )
                    ).use { CookieReader[IO].getSessionToken }

          // Log in as Bob, who is now an existing user.
          redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
          stage2 <- sim.authenticate(redir, Bob, Option(user1).collect { case StandardUser(_, _, _, p) => p.orcidId })
          tok2   <- sso.get(stage2)(CookieReader[IO].getSessionToken)
          user2  <- db.use(_.getStandardUserFromToken(tok2))

          // Ensure the guest token doesn't work anymore
          op     <- db.use(_.findUserFromToken(gtok))

        } yield expect(user1 == user2) && // same user
                expect(op.isEmpty)        // guest user session is invalid
      }
    )
  }

}




