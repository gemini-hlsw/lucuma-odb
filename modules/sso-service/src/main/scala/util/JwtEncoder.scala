// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.util

import cats.ApplicativeThrow
import cats.implicits.*
import pdi.jwt.Jwt
import pdi.jwt.JwtAlgorithm
import pdi.jwt.JwtClaim
import pdi.jwt.algorithms.JwtAsymmetricAlgorithm

import java.security.PrivateKey
import scala.util.control.NonFatal

/** Service for a JWT server that needs to issue tokens. */
trait JwtEncoder[F[_]] {
  def encode(claim: JwtClaim): F[String]
}

object JwtEncoder {

  def apply[F[_]](implicit ev: JwtEncoder[F]): ev.type = ev

  /**
   * Construct a `JwtEncoder` that will encode claims and produce signed tokens using the provided
   * JCA `PrivateKey` and asymmetric key algorithm.
   */
  def withPrivateKey[F[_]: ApplicativeThrow](
    sec: PrivateKey,
    algorithm: JwtAsymmetricAlgorithm = JwtAlgorithm.RS512
  ): JwtEncoder[F] =
    new JwtEncoder[F] {
      def encode(claim: JwtClaim): F[String] =
        try {
          // This will throw if the algorithm isn't available … JCA is awful. So just in case.
          Jwt.encode(claim, sec, algorithm).pure[F]
        } catch {
          case NonFatal(e) => e.raiseError[F, String]
        }
    }


}
