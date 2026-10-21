// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.Sync
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import lucuma.itc.legacy
import lucuma.itc.legacy.codecs.given

import java.lang.reflect.Method

/**
 * This class contains methods to call to methods in ItcCalculation via reflection. This is done
 * because the itc-server runs on scala 3 while the ItcCalculation method is based on scala 2
 *
 * Doing the call via reflection with a custom class loader lets us have both scala versions in the
 * jvm at the same time.
 *
 * Note that params are passed as a String for the same reason avoiding conflicts across classes
 * that may not be compatible. Instead we pass back and forth json encoded version of the params
 * essentially the same as if ITC were a server accepting json and responding json
 */
case class LocalItc[F[_]: {Sync as F}](classLoader: ClassLoader):

  private def resultKey(json: io.circe.Json): Option[io.circe.ACursor] =
    val c = json.hcursor
    c.downField("ItcSpectroscopyResult")
      .success
      .orElse(c.downField("ItcImagingResult").success)

  private def hasAllNullExposureTimes(json: Json): Boolean =
    resultKey(json)
      .exists: result =>
        result
          .downField("times")
          .downField("detectors")
          .as[List[Json]]
          .toOption
          .exists(_.forall(_.hcursor.downField("exposureTime").focus.exists(_.isNull)))

  private def hasAllNullCcdSNRatios(json: Json): Boolean =
    resultKey(json)
      .exists: result =>
        result
          .downField("ccds")
          .as[List[Json]]
          .toOption
          .exists(_.forall(_.hcursor.downField("singleSNRatio").focus.exists(_.isNull)))

  // We need to keep a single reference to the reflected method
  private val calculateGraphsMethod: Method = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculateCharts", classOf[String])

  private val calculateMethod = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    // The ocs calculateExposureTime and calculateSignalToNoise are exactly the same.
    .getMethod("calculateExposureTime", classOf[String])

  private val RightWrapper = "Right("
  private val LeftWrapper  = "Left("

  /**
   * The legacy side returns `Either[String, String].toString`, so the payload arrives wrapped in
   * `Right(...)` or `Left(...)`.
   *
   * Stripping that with substring rather than a regex avoids running a match across the whole
   * result.
   */
  private def unwrap(res: String, wrapper: String): Option[String] =
    Option.when(res.startsWith(wrapper) && res.endsWith(")")):
      res.substring(wrapper.length, res.length - 1)

  private def parseResult[A: Decoder](
    res:        String,
    outOfRange: Json => Boolean
  ): Either[LegacyItcError, A] =
    unwrap(res, RightWrapper) match
      case Some(payload) =>
        parse(payload) match
          case Left(e)                         => LegacyItcError.General(e.getMessage).asLeft
          case Right(json) if outOfRange(json) => LegacyItcError.WavelengthOutOfRange.asLeft
          case Right(json)                     =>
            json.as[A].leftMap(e => LegacyItcError.General(e.getMessage))
      case None          =>
        unwrap(res, LeftWrapper)
          .fold(LegacyItcError.General(res)): payload =>
            decode[LegacyItcError](payload).getOrElse(LegacyItcError.General(payload))
          .asLeft

  /**
   * This method does a call to the method ItcCalculation.calculation via reflection. This is done
   * because the itc-server runs on scala 3 while the ItcCalculation method is based on scala 2
   *
   * Doing the call via reflection with a custom class loader lets us have both scala versions
   * playing in harmony.
   *
   * Note that the param is passed as a String for the same reason avoiding conflicts across classes
   * that may not be compatible. Instead we pass back and forth json encoded version of the params
   * essentially the same as if ITC were a server accepting json and responding json
   */
  def calculateGraphs(jsonParams: String): F[Either[LegacyItcError, GraphsRemoteResult]] =
    F.blocking:
      val res = calculateGraphsMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      parseResult[legacy.GraphsRemoteResult](res, hasAllNullCcdSNRatios)

  def calculateTimeAndGraphs(
    jsonParams: String
  ): F[Either[LegacyItcError, TimeAndGraphsRemoteResult]] =
    F.blocking:
      val res = calculateGraphsMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      parseResult[legacy.TimeAndGraphsRemoteResult](res, hasAllNullCcdSNRatios)

  /**
   * This method does a call to the method ItcCalculation.calculate via reflection.
   */
  def calculate(
    jsonParams: String
  ): F[Either[LegacyItcError, IntegrationTimeRemoteResult]] =
    Sync[F].blocking:
      val res = calculateMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      parseResult[IntegrationTimeRemoteResult](res, hasAllNullExposureTimes)
