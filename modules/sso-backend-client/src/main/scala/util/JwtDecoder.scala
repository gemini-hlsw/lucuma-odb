// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.util

import cats.*
import cats.implicits.*
import pdi.jwt.Jwt
import pdi.jwt.JwtClaim
import pdi.jwt.exceptions.JwtEmptyAlgorithmException
import pdi.jwt.exceptions.JwtEmptySignatureException
import pdi.jwt.exceptions.JwtException
import pdi.jwt.exceptions.JwtExpirationException
import pdi.jwt.exceptions.JwtLengthException
import pdi.jwt.exceptions.JwtNonEmptyAlgorithmException
import pdi.jwt.exceptions.JwtNonEmptySignatureException
import pdi.jwt.exceptions.JwtNonNumberException
import pdi.jwt.exceptions.JwtNonStringException
import pdi.jwt.exceptions.JwtNonStringSetOrStringException
import pdi.jwt.exceptions.JwtNonSupportedAlgorithm
import pdi.jwt.exceptions.JwtNonSupportedCurve
import pdi.jwt.exceptions.JwtNotBeforeException
import pdi.jwt.exceptions.JwtSignatureFormatException
import pdi.jwt.exceptions.JwtValidationException

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
        attemptDecode(token).flatMap {
          case Right(c) => c.pure[F]
          case Left(e)  =>
            e match {
              // N.B. JwtException does not extend Throwable but all its cases do :-\
              case e: JwtLengthException               => ev.raiseError(e)
              case e: JwtValidationException           => ev.raiseError(e)
              case e: JwtSignatureFormatException      => ev.raiseError(e)
              case e: JwtEmptySignatureException       => ev.raiseError(e)
              case e: JwtNonEmptySignatureException    => ev.raiseError(e)
              case e: JwtEmptyAlgorithmException       => ev.raiseError(e)
              case e: JwtNonEmptyAlgorithmException    => ev.raiseError(e)
              case e: JwtExpirationException           => ev.raiseError(e)
              case e: JwtNotBeforeException            => ev.raiseError(e)
              case e: JwtNonSupportedAlgorithm         => ev.raiseError(e)
              case e: JwtNonSupportedCurve             => ev.raiseError(e)
              case e: JwtNonStringException            => ev.raiseError(e)
              case e: JwtNonStringSetOrStringException => ev.raiseError(e)
              case e: JwtNonNumberException            => ev.raiseError(e)
            }
        }

    }

}