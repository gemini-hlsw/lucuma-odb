// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.client.codec.user.*
import lucuma.sso.client.util.JwtDecoder
import org.http4s.*
import org.http4s.Credentials
import org.http4s.Request
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString
import pdi.jwt.Jwt
import pdi.jwt.JwtAlgorithm
import pdi.jwt.JwtClaim

import java.security.KeyPairGenerator
import java.security.SecureRandom
import java.time.Instant
import scala.concurrent.duration.*

trait TestSsoClient:
  import TestSsoClient.* 

  export TestSsoClient.{ authorizationHeader, authorizationObject }

  def validUsers: List[User]

  // User by attachments tests
  val invalidAuthHeader = 
    Authorization(Credentials.Token(AuthScheme.Bearer, "***"))

  def ssoClient: SsoClient[IO, User] =
    new SsoClient.AbstractSsoClient[IO, User]:
      def find(req: Request[IO]): IO[Option[User]] = 
        OptionT.fromOption[IO](req.headers.get[Authorization]).flatMapF(get).value
      def get(authorization: Authorization): IO[Option[User]] =
        authorization match
          case Authorization(Credentials.Token(Bearer, jwt)) =>
            reader
              .decodeClaim(jwt)
              .map(_.getUser)
              .flatMap:
                case Left(t) => IO.raiseError(t)
                case Right(u) => validUsers.find(_ === u).pure[IO]
          case _ => none.pure[IO]
  
private object TestSsoClient:

  private val keyGen     = KeyPairGenerator.getInstance("RSA", "SunRsaSign")
  private val random     = SecureRandom.getInstance("SHA1PRNG", "SUN")
  private val keyPair    = { keyGen.initialize(1024, random); keyGen.generateKeyPair }
  private val decoder    = JwtDecoder.withPublicKey[IO](keyPair.getPublic())
  private val reader     = SsoJwtReader(decoder)
  private val Bearer     = CIString("Bearer")
  private val lucumaUser = "lucuma-user"
  private val padSeconds = 10L
  private val algorithm  = JwtAlgorithm.RS512
  private val jwtTimeout = 1.minute

  export reader.decodeClaim

  private def encode(claim: JwtClaim): IO[String] =
    IO(Jwt.encode(claim, keyPair.getPrivate(), algorithm))

  private val now: IO[Instant] =
    IO(Instant.now)

  private def newClaim(content: String, subject: Option[String], timeout: FiniteDuration): IO[JwtClaim] =
    now.map { inst =>
      JwtClaim(
        content    = content,
        issuer     = Some("lucuma-sso"),
        subject    = subject,
        audience   = Some(Set("lucuma")),
        expiration = Some(inst.plusSeconds(timeout.toSeconds).getEpochSecond),
        notBefore  = Some(inst.getEpochSecond - padSeconds),
        issuedAt   = Some(inst.getEpochSecond - padSeconds),
      )
    }

  private def newClaim(user: User, timeout: FiniteDuration): IO[JwtClaim] =
    newClaim(
      content = Json.obj(lucumaUser -> user.asJson).spaces2,
      subject = Some(user.id.value.toString()),
      timeout = timeout,
    )

  private def newJwt(user: User, timeout: Option[FiniteDuration]): IO[String] =
    newClaim(user, timeout.getOrElse(jwtTimeout)).flatMap(encode)

  /** Map containing Authorization header, for WS. */
  def authorizationObject(user: User): IO[Map[String, Json]] =
    newJwt(user, None).map: jwt =>
      Map("Authorization" -> Json.fromString(s"Bearer $jwt"))

  /** Authorization header, for HTTP. */
  def authorizationHeader(user: User): IO[Authorization] =
    newJwt(user, None).map(jwt => Authorization(Credentials.Token(Bearer, jwt)))



