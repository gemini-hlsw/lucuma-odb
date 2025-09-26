// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import buildinfo.BuildInfo
import cats.effect.Async
import cats.effect.kernel.syntax.all.*
import cats.syntax.all.*
import lucuma.itc.SourceTooBright
import lucuma.itc.UpstreamException
import lucuma.itc.legacy
import natchez.Trace

/**
 * Wraps local calls to ITC to ensure fairness
 */
case class FLocalItc[F[_]: Async: Trace](itcLocal: LocalItc[F]):
  private val F = Async[F]

  val TooBright =
    """This target is too bright for this configuration."""
  val HalfWell  = """The detector well is half filled in (\d*\.?\d*) seconds.""".r

  def calculateGraphs(jsonParams: String): F[GraphsRemoteResult] =
    Trace[F].span("call_legacy graphs"):
      Trace[F].put("method"       -> "calculateGraphs",
                   "params.json"  -> jsonParams,
                   "ocs_git_hash" -> BuildInfo.ocsGitHash
      ) *>
        (F.cede *> itcLocal.calculateGraphs(jsonParams).guarantee(F.cede)).flatMap {
          case Right(result) => F.pure(result)
          case Left(msg)     =>
            msg match {
              case TooBright :: HalfWell(v) :: Nil => F.raiseError(SourceTooBright(BigDecimal(v)))
              case _                               => F.raiseError(new UpstreamException(msg))
            }
        }

  def calculateIntegrationTime(jsonParams: String): F[IntegrationTimeRemoteResult] =
    Trace[F].span("call_legacy integration_time"):
      Trace[F].put("method"       -> "calculateIntegrationTime",
                   "params.json"  -> jsonParams,
                   "ocs_git_hash" -> BuildInfo.ocsGitHash
      ) *>
        (F.cede *> itcLocal.calculateIntegrationTime(jsonParams).guarantee(F.cede)).flatMap {
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
    Trace[F].span("call_legacy signal_to_noise"):
      Trace[F].put("method"       -> "calculateSignalToNoise",
                   "params.json"  -> jsonParams,
                   "ocs_git_hash" -> BuildInfo.ocsGitHash
      ) *>
        (F.cede *> itcLocal.calculateSignalToNoise(jsonParams).guarantee(F.cede)).flatMap {
          case Right(result) => F.pure(result)
          case Left(msg)     =>
            msg match {
              case TooBright :: HalfWell(v) :: Nil =>
                F.raiseError(SourceTooBright(BigDecimal(v)))
              case _                               =>
                F.raiseError(new UpstreamException(msg))
            }
        }
