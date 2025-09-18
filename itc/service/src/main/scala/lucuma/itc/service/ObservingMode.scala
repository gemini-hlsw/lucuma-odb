// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.service.ItcObservationDetails.AnalysisMethod
import lucuma.itc.service.hashes.given
import lucuma.itc.service.syntax.*
import spire.math.Interval
import spire.math.Rational

sealed trait ObservingMode {
  def instrument: Instrument
  def analysisMethod: ItcObservationDetails.AnalysisMethod
}

object ObservingMode {
  given Encoder[ObservingMode] = Encoder.instance {
    case spec: SpectroscopyMode => spec.asJson
    case img: ImagingMode       => img.asJson
  }

  sealed trait SpectroscopyMode extends ObservingMode derives Hash {}

  object SpectroscopyMode {
    given Encoder[ObservingMode.SpectroscopyMode] = Encoder.instance {
      case gn: GmosNorth  => gn.asJson
      case gs: GmosSouth  => gs.asJson
      case f2: Flamingos2 => f2.asJson
    }

    sealed trait GmosSpectroscopy extends SpectroscopyMode derives Hash {
      def isIfu: Boolean

      def resolution: Rational

      def coverage: Interval[Wavelength]

      def centralWavelength: Wavelength

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        if (isIfu)
          ItcObservationDetails.AnalysisMethod.Ifu.Single(
            skyFibres = 250,
            offset = 5.0
          )
        else
          ItcObservationDetails.AnalysisMethod.Aperture.Auto(
            skyAperture = 5.0
          )
    }

    case class GmosNorth(
      centralWavelength: Wavelength,
      disperser:         GmosNorthGrating,
      fpu:               GmosNorthFpuParam,
      filter:            Option[GmosNorthFilter],
      ccdMode:           Option[GmosCcdMode],
      roi:               Option[GmosRoi]
    ) extends GmosSpectroscopy derives Hash {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosNorth

      def resolution: Rational =
        disperser.resolution(centralWavelength, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(centralWavelength).toInterval)(
          (a, b) => a.intersect(b.coverageGN)
        )
    }

    object GmosNorth:
      given Encoder[GmosNorth] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params",
           GmosNSpectroscopyParams(a.centralWavelength, a.disperser, a.fpu, a.filter).asJson
          )
        )

    case class GmosSouth(
      centralWavelength: Wavelength,
      disperser:         GmosSouthGrating,
      fpu:               GmosSouthFpuParam,
      filter:            Option[GmosSouthFilter],
      ccdMode:           Option[GmosCcdMode],
      roi:               Option[GmosRoi]
    ) extends GmosSpectroscopy derives Hash {
      val isIfu = fpu.isIfu

      val instrument: Instrument =
        Instrument.GmosSouth

      def resolution: Rational =
        disperser.resolution(centralWavelength, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(centralWavelength).toInterval)(
          (a, b) => a.intersect(b.coverageGS)
        )
    }

    object GmosSouth:
      given Encoder[GmosSouth] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params",
           GmosSSpectroscopyParams(a.centralWavelength, a.disperser, a.fpu, a.filter).asJson
          )
        )

    case class Flamingos2(
      disperser: Flamingos2Disperser,
      filter:    Flamingos2Filter,
      fpu:       Flamingos2Fpu
    ) extends SpectroscopyMode derives Hash {

      override def analysisMethod: AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val instrument: Instrument =
        Instrument.Flamingos2

    }

    object Flamingos2:
      given Encoder[Flamingos2] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params", Flamingos2SpectroscopyParams(a.disperser, a.fpu, a.filter).asJson)
        )

  }

  sealed trait ImagingMode extends ObservingMode derives Hash

  object ImagingMode {
    given Encoder[ObservingMode.ImagingMode] = Encoder.instance {
      case gn: GmosNorth  => gn.asJson
      case gs: GmosSouth  => gs.asJson
      case f2: Flamingos2 => f2.asJson
    }

    sealed trait GmosImaging extends ImagingMode derives Hash {

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 5.0
        )
    }

    case class GmosNorth(
      filter:  GmosNorthFilter,
      ccdMode: Option[GmosCcdMode]
    ) extends GmosImaging {
      val centralWavelength: Wavelength = Wavelength.Min // Ignored for imaging

      val instrument: Instrument =
        Instrument.GmosNorth

    }

    object GmosNorth:
      given Encoder[GmosNorth] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params", GmosNImagingParams(a.filter).asJson)
        )

    case class GmosSouth(
      filter:  GmosSouthFilter,
      ccdMode: Option[GmosCcdMode]
    ) extends GmosImaging {
      val centralWavelength: Wavelength = Wavelength.Min // Ignored for imaging

      val instrument: Instrument =
        Instrument.GmosSouth
    }

    object GmosSouth:
      given Encoder[GmosSouth] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params", GmosSImagingParams(a.filter).asJson)
        )

    case class Flamingos2(filter: Flamingos2Filter) extends ImagingMode {
      val instrument: Instrument = Instrument.Flamingos2

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )
    }

    object Flamingos2:
      given Encoder[Flamingos2] = a =>
        Json.obj(
          ("instrument", a.instrument.asJson),
          ("params", Flamingos2ImagingParams(a.filter).asJson)
        )
  }

}
