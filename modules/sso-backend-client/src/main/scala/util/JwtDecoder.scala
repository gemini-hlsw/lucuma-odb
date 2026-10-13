// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.util

import cats.*
import cats.implicits.*
import pdi.jwt.Jwt
import pdi.jwt.JwtClaim
import pdi.jwt.exceptions.JwtException

import java.security.PublicKey
import scala.util.Failure
import scala.util.Success

/** Service for a JWT client that needs to verify tokens. */
trait JwtDecoder[F[_]] { outer =>

  /** Attept to decode the given token. */
  def attemptDecode(token: String): F[Either[JwtException, JwtClaim]]

  /** Attept to decode the given token, returning `None` on failure. */
  def decodeOption(token: String): F[Option[JwtClaim]]

  /** Attept to decode the given token, raising an error in F on failure. */
  def decode(token: String): F[JwtClaim]

}

object JwtDecoder {

  /**
   * Construct a JwtDecoder that will decode signed tokens and validate them using the provided
   * JCA `PublicKey`.
   */
  def withPublicKey[F[_]](pub: PublicKey)(
    implicit ev: MonadError[F, Throwable]
  ): JwtDecoder[F] =
    new JwtDecoder[F] {

      def attemptDecode(token: String): F[Either[JwtException, JwtClaim]] =
        Jwt.decode(token, pub) match {
          case Success(c) => c.asRight[JwtException].pure[F]
          case Failure(e: JwtException) => (e: JwtException).asLeft[JwtClaim].pure[F]
          case Failure(e) => ev.raiseError(e)
        }

      def decodeOption(token: String): F[Option[JwtClaim]] =
        attemptDecode(token).map(_.toOption)

      def decode(token: String): F[JwtClaim] =
        attemptDecode(token).flatMap(_.liftTo[F])

    }

}
