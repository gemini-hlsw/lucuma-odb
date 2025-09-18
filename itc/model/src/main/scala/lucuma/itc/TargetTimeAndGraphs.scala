// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.Order
import cats.derived.*
import io.circe.Encoder
import io.circe.generic.semiauto.*
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

case class TargetTimeAndGraphs(
  integrationTime: TargetIntegrationTime,
  graphs:          TargetGraphs
) derives Eq

object TargetTimeAndGraphs:
  given Order[TargetTimeAndGraphs]                                                            = Order.by(_.integrationTime)
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder.AsObject[TargetTimeAndGraphs] =
    deriveEncoder
