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
case class FLocalItc[F[_]: {Async as F, Trace as T}](itcLocal: LocalItc[F]):

  val TooBright =
    """This target is too bright for this configuration."""
  val HalfWell  = """The detector well is half filled in (\d*\.?\d*) seconds.""".r

  def calculateGraphs(jsonParams: String): F[GraphsRemoteResult] =
    T.span("call_legacy graphs"):
      T.put("method"       -> "calculateGraphs",
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
    F.delay(pprint.pprintln(jsonParams)) *>
      T.span("call_legacy integration_time"):
        T.put("method"       -> "calculateIntegrationTime",
              "params.json"  -> jsonParams,
              "ocs_git_hash" -> BuildInfo.ocsGitHash
        ) *>
          (F.cede *> itcLocal.calculateIntegrationTime(jsonParams).guarantee(F.cede)).flatMap {
            case Right(result) =>
              println("==rcrcrccrrcrcrc====")
              pprint.pprintln(result)
              F.pure(result)
            case Left(msg)     =>
              pprint.pprintln(msg)
              msg match {
                case TooBright :: HalfWell(v) :: Nil =>
                  F.raiseError(SourceTooBright(BigDecimal(v)))
                case _                               =>
                  F.raiseError(new UpstreamException(msg))
              }
          }

  def calculateSignalToNoise(jsonParams: String): F[IntegrationTimeRemoteResult] =
    F.delay(pprint.pprintln(jsonParams)) *>
      T.span("call_legacy signal_to_noise"):
        T.put("method"       -> "calculateSignalToNoise",
              "params.json"  -> jsonParams,
              "ocs_git_hash" -> BuildInfo.ocsGitHash
        ) *>
          (F.cede *> itcLocal.calculateSignalToNoise(jsonParams).guarantee(F.cede)).flatMap {
            case Right(result) =>
              pprint.pprintln(result)
              result.pure[F]
            case Left(msg)     =>
              pprint.pprintln(msg)
              msg match {
                case TooBright :: HalfWell(v) :: Nil =>
                  F.raiseError(SourceTooBright(BigDecimal(v)))
                case _                               =>
                  F.raiseError(new UpstreamException(msg))
              }
          }
