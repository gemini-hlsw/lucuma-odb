// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.Sync
import cats.syntax.all.*
import io.circe.parser.decode
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
case class LocalItc[F[_]: Sync](classLoader: ClassLoader):
  // We need to keep a single reference to the reflected method
  private val calculateGraphsMethod: Method = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculateCharts", classOf[String])

  private val calculateExposureTimeMethod = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculateExposureTime", classOf[String])

  private val calculateSignalToNoiseMethod = classLoader
    .loadClass("edu.gemini.itc.web.servlets.ItcCalculation")
    .getMethod("calculateSignalToNoise", classOf[String])

  private val LegacyRight    = """Right\((.*)\)""".r
  private val LegacyLeft     = """Left\(([\s\S]*?)\)""".r
  private val LegacyLeftList = """Left\(List\(([\s\S]*?)\)\)""".r

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
  def calculateGraphs(jsonParams: String): F[Either[List[String], GraphsRemoteResult]] =
    Sync[F].blocking:
      val res = calculateGraphsMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      val result = res match
        case LegacyRight(result)          =>
          decode[legacy.GraphsRemoteResult](result).leftMap { e =>
            List(e.getMessage())
          }
        case LegacyLeft(result)           =>
          Left(List(result))
        case LegacyLeft(result1, result2) =>
          Left(List(result1, result2))
        case m                            =>
          Left(List(m))

      result

  /**
   * This method does a call to the method ItcCalculation.calculate via reflection.
   */
  def calculateIntegrationTime(
    jsonParams: String
  ): F[Either[List[String], IntegrationTimeRemoteResult]] =
    Sync[F].blocking:
      val res = calculateExposureTimeMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      val result = res match
        case LegacyRight(result)    =>
          decode[IntegrationTimeRemoteResult](result).leftMap { e =>
            List(e.getMessage())
          }
        case LegacyLeft(result)     =>
          Left(result.split("\n").toList)
        case LegacyLeftList(result) =>
          Left(result.split("\n").toList)
        case m                      =>
          Left(List(m))

      result

  /**
   * This method does a call to the method ItcCalculation.calculate via reflection.
   */
  def calculateSignalToNoise(
    jsonParams: String
  ): F[Either[List[String], IntegrationTimeRemoteResult]] =
    Sync[F].blocking:
      val res = calculateSignalToNoiseMethod
        .invoke(null, jsonParams) // null as it is a static method
        .asInstanceOf[String]

      val result = res match
        case LegacyRight(result)    =>
          decode[IntegrationTimeRemoteResult](result).leftMap { e =>
            List(e.getMessage())
          }
        case LegacyLeft(result)     =>
          Left(result.split("\n").toList)
        case LegacyLeftList(result) =>
          Left(result.split("\n").toList)
        case m                      =>
          Left(List(m))

      result
