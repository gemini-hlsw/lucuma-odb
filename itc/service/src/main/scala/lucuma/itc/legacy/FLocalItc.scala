// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import buildinfo.BuildInfo
import cats.effect.Async
import cats.effect.kernel.syntax.all.*
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.itc.SourceTooBright
import lucuma.itc.UpstreamException
import lucuma.itc.WavelengthOutOfRange
import lucuma.itc.legacy
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

/**
 * Wraps local calls to ITC to ensure fairness
 */
case class FLocalItc[F[_]: {Async as F, Tracer as T}](itcLocal: LocalItc[F]):

  val TooBright =
    """This target is too bright for this configuration."""
  val HalfWell  = """The detector well is half filled in (\d*\.?\d*) seconds.""".r

  def calculateGraphs(
    jsonParams:   String,
    atWavelength: Wavelength
  ): F[GraphsRemoteResult] =
    T.span("call_legacy graphs",
           Attribute("method", "calculateGraphs"),
           Attribute("params.json", jsonParams),
           Attribute("ocs_git_hash", BuildInfo.ocsGitHash)
    ).surround:
      (F.cede *> itcLocal.calculateGraphs(jsonParams).guarantee(F.cede)).flatMap {
        case Right(result) => F.pure(result)
        case Left(msg)     =>
          msg match {
            case TooBright :: HalfWell(v) :: Nil => F.raiseError(SourceTooBright(BigDecimal(v)))
            case List(LocalItc.OutOfRangeMsg)    =>
              F.raiseError(WavelengthOutOfRange(atWavelength))
            case _                               => F.raiseError(new UpstreamException(msg))
          }
      }

  def calculateTimeAndGraphs(
    jsonParams:   String,
    atWavelength: Wavelength
  ): F[TimeAndGraphsRemoteResult] =
    T.span(
      "call_legacy graphs",
      Attribute("method", "calculateTimeAndGraphs"),
      Attribute("params.json", jsonParams),
      Attribute("ocs_git_hash", BuildInfo.ocsGitHash)
    ).surround:
      (F.cede *> itcLocal.calculateTimeAndGraphs(jsonParams).guarantee(F.cede)).flatMap {
        case Right(result) => F.pure(result)
        case Left(msg)     =>
          msg match {
            case TooBright :: HalfWell(v) :: Nil => F.raiseError(SourceTooBright(BigDecimal(v)))
            case List(LocalItc.OutOfRangeMsg)    =>
              F.raiseError(WavelengthOutOfRange(atWavelength))
            case _                               => F.raiseError(new UpstreamException(msg))
          }
      }

  def calculate(
    jsonParams:   String,
    atWavelength: Wavelength
  ): F[IntegrationTimeRemoteResult] =
    F.delay(pprint.pprintln(jsonParams)) *>
      T.span("call_legacy calculate",
             Attribute("method", "calculate"),
             Attribute("params.json", jsonParams),
             Attribute("ocs_git_hash", BuildInfo.ocsGitHash)
      ).surround:
        (F.cede *> itcLocal.calculate(jsonParams).guarantee(F.cede)).flatMap {
          case Right(result) =>
            F.pure(result)
          case Left(msg)     =>
            msg match {
              case TooBright :: HalfWell(v) :: Nil =>
                F.raiseError(SourceTooBright(BigDecimal(v)))
              case List(LocalItc.OutOfRangeMsg)    =>
                F.raiseError(WavelengthOutOfRange(atWavelength))
              case _                               =>
                F.raiseError(new UpstreamException(msg))
            }
        }
