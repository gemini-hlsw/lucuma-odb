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
import lucuma.core.util.TimeSpan

/**
 * The exposure time of a single exposure and the number of frames to take. A frame is what the
 * detector delivers: `coadds` exposures summed on chip. Without coadds a frame is one exposure, so
 * the frame count is also the exposure count.
 */
case class IntegrationTime(
  exposureTime: TimeSpan,
  frameCount:   PosInt
)

object IntegrationTime:
  // The brightest target will be the one with the smallest exposure time.
  // We break ties by frame count.
  given Order[IntegrationTime] = Order.by(it => (it.exposureTime, it.frameCount))

  // `exposureCount` is the deprecated GraphQL name of `frameCount`.
  given (using Encoder[TimeSpan]): Encoder[IntegrationTime] = it =>
    Json.obj(
      "exposureTime"  -> it.exposureTime.asJson,
      "frameCount"    -> it.frameCount.asJson,
      "exposureCount" -> it.frameCount.asJson
    )
