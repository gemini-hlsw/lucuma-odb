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
 * The exposure time of a single exposure, the number of frames to take and the coadds per frame. A
 * frame is what the detector delivers: `coadds` exposures summed on chip. Without coadds a frame is
 * one exposure, so the frame count is also the exposure count. In S/N mode the ITC chooses the
 * coadds; in time-and-count mode they are the requested ones.
 */
case class IntegrationTime(
  exposureTime: TimeSpan,
  frameCount:   PosInt,
  coadds:       PosInt = PosInt.unsafeFrom(1)
):
  /** Single exposures over the whole result: every frame's coadds. */
  def totalExposureCount: PosInt =
    PosInt.unsafeFrom(frameCount.value * coadds.value)

object IntegrationTime:
  // The brightest target will be the one with the smallest exposure time.
  // We break ties by coadds, then frame count.
  given Order[IntegrationTime] = Order.by(it => (it.exposureTime, it.coadds, it.frameCount))

  // `exposureCount` is the deprecated GraphQL name of `frameCount`.
  given (using Encoder[TimeSpan]): Encoder[IntegrationTime] = it =>
    Json.obj(
      "exposureTime"  -> it.exposureTime.asJson,
      "frameCount"    -> it.frameCount.asJson,
      "coadds"        -> it.coadds.asJson,
      "exposureCount" -> it.frameCount.asJson
    )
