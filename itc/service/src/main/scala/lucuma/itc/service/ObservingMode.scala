// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.ItcGhostDetector
import lucuma.itc.service.ItcObservationDetails.AnalysisMethod
import lucuma.itc.service.hashes.given
import lucuma.itc.service.syntax.*
import spire.math.Interval
import spire.math.Rational

sealed trait ObservingMode {
  def instrument: Instrument
  def analysisMethod: ItcObservationDetails.AnalysisMethod
  def portDisposition: PortDisposition

  def description: String
}

object ObservingMode {
  sealed trait SpectroscopyMode extends ObservingMode derives Hash {}

  object SpectroscopyMode {
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
      roi:               Option[GmosRoi],
      portDisposition:   PortDisposition
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

      val description: String =
        s"${instrument.shortName} Longslit"
    }

    case class GmosSouth(
      centralWavelength: Wavelength,
      disperser:         GmosSouthGrating,
      fpu:               GmosSouthFpuParam,
      filter:            Option[GmosSouthFilter],
      ccdMode:           Option[GmosCcdMode],
      roi:               Option[GmosRoi],
      portDisposition:   PortDisposition
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

      val description: String =
        s"${instrument.shortName} Longslit"
    }

    case class Flamingos2(
      disperser:       Flamingos2Disperser,
      filter:          Flamingos2Filter,
      readMode:        Flamingos2ReadMode,
      fpu:             Flamingos2Fpu,
      portDisposition: PortDisposition
    ) extends SpectroscopyMode derives Hash {

      override def analysisMethod: AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val instrument: Instrument =
        Instrument.Flamingos2

      val description: String =
        s"${instrument.shortName} Longslit"
    }

    case class Igrins2(portDisposition: PortDisposition) extends SpectroscopyMode derives Hash {
      override def analysisMethod: AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val instrument: Instrument =
        Instrument.Igrins2

      val description: String =
        s"${instrument.shortName} Longslit"
    }

    case class Ghost(
      numSkyMicrolens: Int,
      resolutionMode:  GhostResolutionMode,
      redDetector:     ItcGhostDetector,
      blueDetector:    ItcGhostDetector
    ) extends SpectroscopyMode derives Hash {

      val instrument: Instrument =
        Instrument.Ghost

      def portDisposition: PortDisposition =
        PortDisposition.Bottom

      override def analysisMethod: AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Ifu.Sky(
          skyFibres = numSkyMicrolens
        )

      val description: String =
        s"${instrument.shortName} IFU"
    }
  }

  sealed trait ImagingMode extends ObservingMode derives Hash

  object ImagingMode {

    sealed trait GmosImaging extends ImagingMode derives Hash {

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 5.0
        )
    }

    case class GmosNorth(
      filter:          GmosNorthFilter,
      ccdMode:         Option[GmosCcdMode],
      portDisposition: PortDisposition
    ) extends GmosImaging {
      val centralWavelength: Wavelength = Wavelength.Min // Ignored for imaging

      val instrument: Instrument =
        Instrument.GmosNorth

      val description: String =
        s"${instrument.shortName} Imaging"
    }

    case class GmosSouth(
      filter:          GmosSouthFilter,
      ccdMode:         Option[GmosCcdMode],
      portDisposition: PortDisposition
    ) extends GmosImaging {
      val centralWavelength: Wavelength = Wavelength.Min // Ignored for imaging

      val instrument: Instrument =
        Instrument.GmosSouth

      val description: String =
        s"${instrument.shortName} Imaging"
    }

    case class Flamingos2(
      filter:          Flamingos2Filter,
      readMode:        Flamingos2ReadMode,
      portDisposition: PortDisposition
    ) extends ImagingMode {
      val instrument: Instrument = Instrument.Flamingos2

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val description: String =
        s"${instrument.shortName} Imaging"
    }
  }
}
