// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.test

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.util.Gid
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.codec.user.given
import lucuma.sso.client.util.JwtDecoder
import org.http4s.*
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import pdi.jwt.Jwt
import pdi.jwt.JwtAlgorithm
import pdi.jwt.JwtClaim

import java.security.KeyPairGenerator
import java.security.PublicKey
import java.security.SecureRandom
import java.time.Instant
import scala.concurrent.duration.*

object TestSso:

  private val keyGen  = KeyPairGenerator.getInstance("RSA", "SunRsaSign")
  private val random  = SecureRandom.getInstance("SHA1PRNG", "SUN")
  private val keyPair = { keyGen.initialize(2048, random); keyGen.generateKeyPair }

  val publicKey: PublicKey = keyPair.getPublic

  private val Bearer     = CIString("Bearer")
  private val lucumaUser = "lucuma-user"
  private val padSeconds = 10L
  private val algorithm  = JwtAlgorithm.RS512
  private val jwtTimeout = 1.minute

  private val reader: SsoJwtReader[IO] =
    SsoJwtReader(JwtDecoder.withPublicKey[IO](publicKey))

  def ssoClient: SsoClient[IO, User] =
    new SsoClient.AbstractSsoClient[IO, User]:
      def find(req: Request[IO]): IO[Option[User]]            =
        OptionT.fromOption[IO](req.headers.get[Authorization]).flatMapF(get).value
      def get(authorization: Authorization): IO[Option[User]] =
        authorization match
          case Authorization(Credentials.Token(Bearer, jwt)) =>
            reader.decodeClaim(jwt).map(_.getUser.toOption).attempt.map(_.toOption.flatten)
          case _                                             =>
            none.pure[IO]

  /** A garden-variety authenticated user. */
  val pi: User =
    standardUser(11, 110)

  def standardUser(id: Long, roleId: Long): StandardUser =
    StandardUser(
      id = Gid[User.Id].fromLong.getOption(id).get,
      role = StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(roleId).get),
      otherRoles = Nil,
      profile = OrcidProfile(
        orcidId = orcidId(id),
        profile = UserProfile(None, None, None, None)
      )
    )

  private def orcidId(id: Long): OrcidId =
    val ds           = f"$id%015d"
    val (a, b, c, d) =
      (ds.substring(0, 4), ds.substring(4, 8), ds.substring(8, 12), ds.substring(12, 15))
    val x            = checkDigit(a + b + c + d)
    OrcidId.fromValue(s"$a-$b-$c-$d$x").fold(sys.error, identity)

  private def checkDigit(baseDigits: String): String =
    val total     = baseDigits.foldLeft(0)((acc, c) => (acc + (c - '0')) * 2)
    val remainder = total            % 11
    val result    = (12 - remainder) % 11
    if result == 10 then "X" else result.toString

  private def encode(claim: JwtClaim): IO[String] =
    IO(Jwt.encode(claim, keyPair.getPrivate, algorithm))

  private def newClaim(user: User, timeout: FiniteDuration): IO[JwtClaim] =
    IO(Instant.now).map: inst =>
      JwtClaim(
        content = Json.obj(lucumaUser -> user.asJson).spaces2,
        issuer = Some("lucuma-sso"),
        subject = Some(user.id.value.toString),
        audience = Some(Set("lucuma")),
        expiration = Some(inst.plusSeconds(timeout.toSeconds).getEpochSecond),
        notBefore = Some(inst.getEpochSecond - padSeconds),
        issuedAt = Some(inst.getEpochSecond - padSeconds)
      )

  private def newJwt(user: User): IO[String] =
    newClaim(user, jwtTimeout).flatMap(encode)

  /** Authorization header carrying a valid JWT for `user`. */
  def authorizationHeader(user: User): IO[Authorization] =
    newJwt(user).map(jwt => Authorization(Credentials.Token(Bearer, jwt)))
