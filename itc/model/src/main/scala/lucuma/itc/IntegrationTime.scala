// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Order
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.generic.semiauto.*
import io.circe.refined.*
import lucuma.core.util.TimeSpan

case class IntegrationTime(
  exposureTime:  TimeSpan,
  exposureCount: PosInt
)

object IntegrationTime:
  // The brightest target will be the one with the smallest exposure time.
  // We break ties by exposure count.
  given Order[IntegrationTime] = Order.by(it => (it.exposureTime, it.exposureCount))

  given (using Encoder[TimeSpan]): Encoder[IntegrationTime] = deriveEncoder
