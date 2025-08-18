// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.numeric.Positive
import lucuma.core.model.ServiceUser
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.sso.client.ApiKey
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoClient.UserInfo
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.headers.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString

import scala.concurrent.duration.*

object SsoClientSuite extends SsoSuite with Fixture with FlakyTests {
  inline given Predicate[Long, Positive] with
    transparent inline def isValid(inline t: Long): Boolean = t > 0

  def routes(ssoClient: SsoClient[IO, UserInfo]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case r@(GET -> Root / "echo-name") =>
        ssoClient.map(_.user).require(r) { u =>
          Ok(u.displayName)
        }
    }

  def otherServer(ssoClient: SsoClient[IO, UserInfo]): Client[IO] =
    Client.fromHttpApp(routes(ssoClient).orNotFound)

  test("Call a remote service with a JWT.") {
    flaky()(
      SsoSimulator[IO].use { case (_, sim, sso, reader, writer) =>
        val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
        val Bearer = CIString("Bearer")
        for {

          // Create our SSO Client
          serviceJwt <- writer.newJwt(ServiceUser(User.Id(1L.refined), "bogus")) // need to call as a service user
          ssoClient  <- SsoClient.initial[IO](
            httpClient = sso,
            ssoRoot = uri"http://ignored",
            jwtReader = reader,
            gracePeriod = 5.minutes,
            serviceJwt = serviceJwt
          )
          other = otherServer(ssoClient)

          // Log in as Bob
          redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
          stage2 <- sim.authenticate(redir, Bob, None)
          _      <- sso.get(stage2)(CookieReader[IO].getSessionToken) // cookie is set here
          jwt    <- sso.expect[String](Request[IO](method = Method.POST, uri = SsoRoot / "api" / "v1" / "refresh-token"))

          // Call the other server!
          name   <- other.expect[String](
                      Request[IO](
                        uri = uri"http://whatever.com/echo-name",
                        headers = Headers(Authorization(Credentials.Token(Bearer, jwt)))
                      )
                    )

        } yield expect(name == "Bob Dobbs")
      }
    )
  }

  test("Call a remote service with an API key.") {
    flaky()(
      SsoSimulator[IO].use { case (db, sim, sso, reader, writer) =>
        val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
        val Bearer = CIString("Bearer")
        for {

          // Create our SSO Client
          serviceJwt <- writer.newJwt(ServiceUser(User.Id(1L.refined), "bogus")) // need to call as a service user
          ssoClient  <- SsoClient.initial[IO](
            httpClient  = sso,
            ssoRoot     = SsoRoot,
            jwtReader   = reader,
            gracePeriod = 5.minutes,
            serviceJwt  = serviceJwt
          )
          other = otherServer(ssoClient)

          // Log in as Bob
          redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
          stage2 <- sim.authenticate(redir, Bob, None)
          tok    <- sso.get(stage2)(CookieReader[IO].getSessionToken) // cookie is set here
          user   <- db.use(_.getStandardUserFromToken(tok))
          apiKey <- db.use(_.createApiKey(user.role.id))

          // Call the other server!
          name   <- other.expect[String](
                      Request[IO](
                        uri = uri"http://whatever.com/echo-name",
                        headers = Headers(Authorization(Credentials.Token(Bearer, ApiKey.fromString.reverseGet(apiKey))))
                      )
                    )

        } yield expect(name == "Bob Dobbs")
      }
    )
  }


  test("Can't call remote service with no user.") {
    flaky()(
      SsoSimulator[IO].use { case (_, sim, sso, reader, writer) =>
        val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
        for {

          // Create our SSO Client
          serviceJwt <- writer.newJwt(ServiceUser(User.Id(1L.refined), "bogus")) // need to call as a service user
          ssoClient  <- SsoClient.initial[IO](
            httpClient  = sso,
            ssoRoot     = SsoRoot,
            jwtReader   = reader,
            gracePeriod = 5.minutes,
            serviceJwt  = serviceJwt
          )
          other = otherServer(ssoClient)

          // Log in as Bob
          redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
          stage2 <- sim.authenticate(redir, Bob, None)
          _      <- sso.get(stage2)(CookieReader[IO].getSessionToken) // cookie is set here

          // Call the other server!
          status <- other.status(
                      Request[IO](
                        uri = uri"http://whatever.com/echo-name",
                      )
                    )

        } yield expect(status == Status.Forbidden)
      }
    )
  }

}
