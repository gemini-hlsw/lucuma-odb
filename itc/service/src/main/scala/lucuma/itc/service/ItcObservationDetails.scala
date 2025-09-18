// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.service.syntax.finiteduration.*

import scala.concurrent.duration.FiniteDuration

case class ItcObservationDetails(
  calculationMethod: ItcObservationDetails.CalculationMethod,
  analysisMethod:    ItcObservationDetails.AnalysisMethod
)

object ItcObservationDetails {

  sealed trait CalculationMethod extends Product with Serializable

  object CalculationMethod {

    // Methods that return signla to noise from exp time/count
    sealed trait S2NMethod extends CalculationMethod
    object S2NMethod {

      case class ImagingS2N(
        exposureCount:    Int,
        coadds:           Option[Int],
        exposureDuration: FiniteDuration,
        sourceFraction:   Double,
        ditherOffset:     Angle
      ) extends S2NMethod

      object ImagingS2N:
        val encoder: Encoder[ImagingS2N] = a =>
          Json.obj(
            "exposures"      -> a.exposureCount.asJson,
            "coadds"         -> a.coadds.asJson,
            "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
            "sourceFraction" -> a.sourceFraction.asJson,
            "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson
          )

      case class SpectroscopyS2N(
        exposureCount:    Int,
        coadds:           Option[Int],
        exposureDuration: FiniteDuration,
        sourceFraction:   Double,
        ditherOffset:     Angle,
        wavelengthAt:     Wavelength
      ) extends S2NMethod

      object SpectroscopyS2N:
        val encoder: Encoder[SpectroscopyS2N] = a =>
          Json.obj(
            "exposures"      -> a.exposureCount.asJson,
            "coadds"         -> a.coadds.asJson,
            "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
            "sourceFraction" -> a.sourceFraction.asJson,
            "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson,
            "wavelengthAt"   -> a.wavelengthAt.nm.value.value.asJson
          )

      given encoder: Encoder[S2NMethod] = a =>
        a match {
          case a: SpectroscopyS2N => Json.obj("SpectroscopyS2N" -> SpectroscopyS2N.encoder(a))
          case a: ImagingS2N      => Json.obj("ImagingS2N" -> ImagingS2N.encoder(a))
        }

    }

    sealed trait IntegrationTimeMethod extends CalculationMethod
    object IntegrationTimeMethod {
      case class SpectroscopyIntegrationTime(
        sigma:          Double,
        coadds:         Option[Int],
        sourceFraction: Double,
        ditherOffset:   Angle,
        wavelengthAt:   Wavelength
      ) extends IntegrationTimeMethod

      object SpectroscopyIntegrationTime:
        val encoder: Encoder[SpectroscopyIntegrationTime] = a =>
          Json.obj(
            "sigma"          -> a.sigma.asJson,
            "coadds"         -> a.coadds.asJson,
            "sourceFraction" -> a.sourceFraction.asJson,
            "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson,
            "wavelengthAt"   -> a.wavelengthAt.nm.value.value.asJson
          )

      case class ImagingIntegrationTime(
        sigma:          Double,
        coadds:         Option[Int],
        sourceFraction: Double,
        ditherOffset:   Angle
      ) extends IntegrationTimeMethod

      object ImagingIntegrationTime:
        val encoder: Encoder[ImagingIntegrationTime] = a =>
          Json.obj(
            "sigma"          -> a.sigma.asJson,
            "coadds"         -> a.coadds.asJson,
            "sourceFraction" -> a.sourceFraction.asJson,
            "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson
          )

      // We expect a spectroscopy option at some point
      val encoder: Encoder[IntegrationTimeMethod] = a =>
        a match {
          case a: SpectroscopyIntegrationTime =>
            Json.obj("SpectroscopyIntegrationTime" -> SpectroscopyIntegrationTime.encoder(a))
          case a: ImagingIntegrationTime      =>
            Json.obj("ImagingIntegrationTime" -> ImagingIntegrationTime.encoder(a))
        }
    }

    case class ImagingExposureCount(
      sigma:            Double,
      exposureDuration: FiniteDuration,
      coadds:           Option[Int],
      sourceFraction:   Double,
      ditherOffset:     Angle
    ) extends CalculationMethod

    object ImagingExposureCount:
      val encoder: Encoder[ImagingExposureCount] = a =>
        Json.obj(
          "sigma"          -> a.sigma.asJson,
          "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
          "coadds"         -> a.coadds.asJson,
          "sourceFraction" -> a.sourceFraction.asJson,
          "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson
        )

    given Encoder[CalculationMethod] = a =>
      a match {
        case a: S2NMethod             => Json.obj("S2NMethod" -> S2NMethod.encoder(a))
        case a: IntegrationTimeMethod =>
          Json.obj("IntegrationTimeMethod" -> IntegrationTimeMethod.encoder(a))
        case a: ImagingExposureCount  =>
          Json.obj("ExposureCountMethod" -> ImagingExposureCount.encoder(a))
      }

  }

  sealed trait AnalysisMethod extends Product with Serializable

  object AnalysisMethod {

    sealed trait Aperture extends AnalysisMethod
    object Aperture {

      case class Auto(skyAperture: Double) extends Aperture

      object Auto {
        val encoder: Encoder[Auto] = deriveEncoder
      }

      case class User(diameter: Double, skyAperture: Double) extends Aperture

      object User {
        val encoder: Encoder[User] = deriveEncoder
      }

      given encoder: Encoder[Aperture] = a =>
        a match {
          case a: Auto => Json.obj("AutoAperture" -> Auto.encoder(a))
          case a: User => Json.obj("UserAperture" -> User.encoder(a))
        }

    }

    sealed trait Ifu extends AnalysisMethod
    object Ifu {

      case class Single(skyFibres: Int, offset: Double) extends Ifu

      object Single {
        val encoder: Encoder[Single] = deriveEncoder
      }

      case class Radial(skyFibres: Int, minOffset: Double, maxOffset: Double) extends Ifu

      object Radial {
        val encoder: Encoder[Radial] = deriveEncoder
      }

      case class Summed(
        skyFibres: Int,
        numX:      Int,
        numY:      Int,
        centerX:   Double,
        centerY:   Double
      ) extends Ifu

      object Summed {
        val encoder: Encoder[Summed] = deriveEncoder
      }

      case class Sum(skyFibres: Int, num: Double, isIfu2: Boolean) extends Ifu

      object Sum {
        val encoder: Encoder[Sum] = deriveEncoder
      }

      val encoder: Encoder[Ifu] = a =>
        a match {
          case a: Single => Json.obj("IfuSingle" -> Single.encoder(a))
          case a: Radial => Json.obj("IfuRadial" -> Radial.encoder(a))
          case a: Summed => Json.obj("IfuSummed" -> Summed.encoder(a))
          case a: Sum    => Json.obj("IfuSum" -> Sum.encoder(a))
        }

    }

    given Encoder[AnalysisMethod] = a =>
      a match {
        case a: Aperture => Json.obj("ApertureMethod" -> Aperture.encoder(a))
        case a: Ifu      => Json.obj("IfuMethod" -> Ifu.encoder(a))
      }

  }

  given Encoder[ItcObservationDetails] =
    deriveEncoder

}
