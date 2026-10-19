// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Order
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.util.TimeSpan

/**
 * The exposure time of a single exposure, the number of frames to take, the coadds per frame and
 * the read mode. A frame is what the detector delivers: `coadds` exposures summed on chip. Without
 * coadds a frame is one exposure, so the frame count is also the exposure count. In S/N mode the
 * ITC chooses the coadds and the read mode; in time-and-count mode they are the requested ones. The
 * read mode is `None` for instruments whose recipe does not select one.
 */
case class IntegrationTime(
  exposureTime: TimeSpan,
  frameCount:   PosInt,
  coadds:       PosInt = PosInt.unsafeFrom(1),
  readMode:     Option[ItcReadMode] = None
):
  def gnirsReadMode: Option[GnirsReadMode] =
    readMode.collect:
      case ItcReadMode.Gnirs(readMode = r) => r

  def flamingos2ReadMode: Option[Flamingos2ReadMode] =
    readMode.collect:
      case ItcReadMode.Flamingos2(readMode = r) => r

  /**
   * Single exposures over the whole result: every frame's coadds. Saturates rather than
   * overflowing; a count that large is unusable anyway.
   */
  def totalExposureCount: PosInt =
    PosInt.unsafeFrom((frameCount.value.toLong * coadds.value).min(Int.MaxValue).toInt)

object IntegrationTime:
  // The brightest target will be the one with the smallest exposure time. We break ties by
  // the single exposures needed in total, then by the components for determinism.
  given Order[IntegrationTime] =
    Order.by(it => (it.exposureTime, it.totalExposureCount, it.coadds, it.frameCount))

  // `exposureCount` is the deprecated GraphQL name of `frameCount`.
  given (using Encoder[TimeSpan]): Encoder[IntegrationTime] = it =>
    Json.obj(
      "exposureTime"       -> it.exposureTime.asJson,
      "frameCount"         -> it.frameCount.asJson,
      "coadds"             -> it.coadds.asJson,
      "exposureCount"      -> it.frameCount.asJson,
      "gnirsReadMode"      -> it.gnirsReadMode.asJson,
      "flamingos2ReadMode" -> it.flamingos2ReadMode.asJson
    )
