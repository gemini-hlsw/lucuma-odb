// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.Async
import cats.effect.kernel.syntax.all.*
import cats.syntax.all.*
import lucuma.itc.SourceTooBright
import lucuma.itc.UpstreamException
import lucuma.itc.legacy

/**
 * Wraps local calls to ITC to ensure fairness
 */
case class FLocalItc[F[_]: Async](itcLocal: LocalItc):
  private val F = Async[F]

  val TooBright =
    """This target is too bright for this configuration."""
  val HalfWell  = """The detector well is half filled in (\d*\.?\d*) seconds.""".r

  def calculateGraphs(jsonParams: String): F[GraphsRemoteResult] =
    (F.cede *> F.delay(itcLocal.calculateGraphs(jsonParams)).guarantee(F.cede)).flatMap {
      case Right(result) => F.pure(result)
      case Left(msg)     =>
        msg match {
          case TooBright :: HalfWell(v) :: Nil => F.raiseError(SourceTooBright(BigDecimal(v)))
          case _                               => F.raiseError(new UpstreamException(msg))
        }
    }

  def calculateIntegrationTime(jsonParams: String): F[IntegrationTimeRemoteResult] =
    (F.cede *> F.delay(itcLocal.calculateIntegrationTime(jsonParams)).guarantee(F.cede)).flatMap {
      case Right(result) => F.pure(result)
      case Left(msg)     =>
        msg match {
          case TooBright :: HalfWell(v) :: Nil =>
            F.raiseError(SourceTooBright(BigDecimal(v)))
          case _                               =>
            F.raiseError(new UpstreamException(msg))
        }
    }

  def calculateSignalToNoise(jsonParams: String): F[IntegrationTimeRemoteResult] =
    (F.cede *> F.delay(itcLocal.calculateSignalToNoise(jsonParams)).guarantee(F.cede)).flatMap {
      case Right(result) => F.pure(result)
      case Left(msg)     =>
        msg match {
          case TooBright :: HalfWell(v) :: Nil =>
            F.raiseError(SourceTooBright(BigDecimal(v)))
          case _                               =>
            F.raiseError(new UpstreamException(msg))
        }
    }
