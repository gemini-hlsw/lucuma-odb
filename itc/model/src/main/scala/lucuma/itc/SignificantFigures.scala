// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import eu.timepit.refined.types.numeric.PosInt
import io.circe.*
import io.circe.refined.*

import scala.math.*

case class SignificantFigures(xAxis: Option[PosInt], yAxis: Option[PosInt], ccd: Option[PosInt])
    derives Encoder.AsObject,
      Decoder
