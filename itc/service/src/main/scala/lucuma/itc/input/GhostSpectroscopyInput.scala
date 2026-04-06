// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.ItcGhostDetector
import lucuma.odb.graphql.binding.*

case class GhostSpectroscopyInput(
  numSkyMicrolens: Int,
  resolutionMode:  GhostResolutionMode,
  redDetector:     ItcGhostDetector,
  blueDetector:    ItcGhostDetector
) extends InstrumentModesInput:
  val port: PortDisposition              = PortDisposition.Bottom
  // This will not be used by the OCS ITC, but is required to meet the API
  val exposureTimeMode: ExposureTimeMode = redDetector.timeAndCount

object GhostSpectroscopyInput:
  // Move to core for easier validation by clients?
  extension (rm: GhostResolutionMode)
    def allowedSkyMicrolens: List[Int] =
      rm match
        case GhostResolutionMode.Standard => List(3, 7, 10)
        case GhostResolutionMode.High     => List(7)
    def defaultSkyMicrolens: Int       =
      rm match
        case GhostResolutionMode.Standard => 3
        case GhostResolutionMode.High     => 7

  private def validateNumSkyMicrolens(
    oNumSkyMicrolens: Option[Int],
    resolutionMode:   GhostResolutionMode
  ): Result[Int] =
    oNumSkyMicrolens.fold(Result(resolutionMode.defaultSkyMicrolens)): numSkyMicrolens =>
      if (resolutionMode.allowedSkyMicrolens.contains(numSkyMicrolens))
        Result(numSkyMicrolens)
      else
        Result.failure(
          s"Invalid number of sky microlenses for ${resolutionMode.shortName} resolution mode. Allowed values are: ${resolutionMode.allowedSkyMicrolens.mkString(", ")}."
        )

  val Binding: Matcher[GhostSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            IntBinding.Option("numSkyMicrolens", oNumSkyMicrolens),
            GhostResolutionModeBinding("resolutionMode", resolutionMode),
            ItcGhostDetectorInput.Binding("redDetector", redDetector),
            ItcGhostDetectorInput.Binding("blueDetector", blueDetector)
          ) =>
        (oNumSkyMicrolens, resolutionMode, redDetector, blueDetector).parTupled
          .flatMap: (oNumSky, rm, rd, bd) =>
            validateNumSkyMicrolens(oNumSky, rm)
              .map(numSky => GhostSpectroscopyInput(numSky, rm, rd, bd))
    }
