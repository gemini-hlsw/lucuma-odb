// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package issue.shortcut


import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.util.Gid
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.util.JwtDecoder
import lucuma.sso.service.config.Config
import lucuma.sso.service.orcid.OrcidIdGenerator
import lucuma.sso.service.simulator.SsoSimulator
import lucuma.sso.service.util.JwtEncoder
import monocle.Prism
import org.http4s.*
import org.http4s.headers.Location
import weaver.SimpleMutableIOSuite

import scala.concurrent.duration.*

object Shortcut_3485 extends SsoSuite with Fixture with OrcidIdGenerator[IO]:

  extension [S,A](p: Prism[S,A]) def unsafeGet(s: S): A =
    p.getOption(s).get
  

  test("generate and then decode a JWT, ensuring that email is preserved"):

    val config     = Config.local(null, null)  
    val jwtEncoder = JwtEncoder.withPrivateKey(config.privateKey)
    val jwtDecoder = JwtDecoder.withPublicKey(config.publicKey)
    val jwtWriter  = SsoJwtWriter(jwtEncoder, 1.minute)
    val jwtReader  = SsoJwtReader(jwtDecoder)

    val userId = Gid[User.Id].fromLong.unsafeGet(123)
    val roleId = Gid[StandardRole.Id].fromLong.unsafeGet(456)
    val email  = "bob@dole.com"

    def user(orcidId: OrcidId) =
      StandardUser(
        id = userId,
        role = StandardRole.Pi(roleId),
        otherRoles = Nil,
        profile = OrcidProfile(
          orcidId = orcidId,
          profile = UserProfile(
            givenName  = Some("Bob"),
            familyName = Some("Dole"),
            creditName = None,
            email      = Some(email)
          )
        )
      )

    for 
      orcidId <- randomOrcidId
      jwt     <- jwtWriter.newJwt(user(orcidId))
      decoded <- jwtReader.decodeStandardUser(jwt)
    yield expect(decoded.profile.email == Some(email))


  test("ensure generated JWT for existing user includes email."):
    SsoSimulator[IO].use:
      case (db, sim, sso, reader, _) =>

        implicit val entityDecoder: EntityDecoder[IO, StandardUser] =
          EntityDecoder.text[IO].flatMapR { token =>
            EitherT(reader.decodeStandardUser(token).attempt)
              .leftMap {
                case e: Exception => InvalidMessageBodyFailure(s"Invalid or missing JWT, or not a standard user.", Some(e))
              }
          }

        val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)

        for
          res    <- sso.get(stage1)(_.pure[IO])
          _      <- expect(res.status == Status.Found).failFast
          loc     = res.headers.get[Location].map(_.uri)
          _      <- expect(loc.isDefined).failFast
          stage2 <- sim.authenticate(loc.get, Bob, None)
          _      <- sso.get(stage2)(CookieReader[IO].getSessionToken)
          bob    <- sso.fetchAs[StandardUser](Request[IO](Method.POST, SsoRoot / "api" / "v1" / "refresh-token"))
        yield expect(bob.profile.email.get === Bob.primaryEmail.get.email)
