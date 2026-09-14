// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.model.ElevationRange

/**
 * Image quality as the legacy ITC takes it: either the delivered FWHM at the science wavelength or
 * one of its percentile bins, which it scales itself with wavelength and airmass. For the moment,
 * we only need the 20% percentile, used for ALTAIR LGS+P1.
 */
enum ItcImageQuality derives Hash:
  case Exact(arcsec: BigDecimal)

  /** The 20% bin, the delivered image quality assumed for Altair LGS+P1. */
  case Percentile20

case class ItcObservingConditions(
  iq:      ItcImageQuality,
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
