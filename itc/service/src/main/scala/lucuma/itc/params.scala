// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.model.ElevationRange

case class ItcObservingConditions(
  iq:      BigDecimal,
  cc:      BigDecimal,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
) derives Hash

object ItcObservingConditions:

  val AirMassBuckets = Vector(BigDecimal(1.2), BigDecimal(1.5), BigDecimal(2.0))

  def airmass(er: ElevationRange): Either[String, BigDecimal] =
    er match
      case ElevationRange.ByAirMass(min, max) if max.toBigDecimal >= min.toBigDecimal   =>
        AirMassBuckets.find(max.toBigDecimal <= _).getOrElse(AirMassBuckets.last).asRight
      case ElevationRange.ByAirMass(min, max)                                           =>
        Left("Maximum airmass must be greater than minimum airmass")
      case ElevationRange.ByHourAngle(min, max) if max.toBigDecimal >= min.toBigDecimal =>
        max.toBigDecimal.asRight
      case ElevationRange.ByHourAngle(min, max)                                         =>
        Left(s"Hour Angle max value $max must be more than the min value $min")
