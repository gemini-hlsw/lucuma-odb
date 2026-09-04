// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gnirs.GnirsFpu
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

      /** How the focal plane unit gets its sky, when it is an IFU. */
      def ifuSky: Option[GmosIfuSky]

      /** Whether the legacy recipe treats the focal plane unit as the two-slit IFU. */
      def isTwoSlitIfu: Boolean

      /** How to sample the IFU field. Ignored unless the focal plane unit is an IFU. */
      def ifuAnalysis: Option[GmosIfuAnalysis]

      def resolution: Rational

      def coverage: Interval[Wavelength]

      def centralWavelength: Wavelength

      // Whether this is an IFU at all follows from the focal plane unit; `ifuAnalysis` only
      // refines how the field is sampled, so a caller who omits it still gets a usable IFU
      // calculation. The legacy recipe rejects the two halves disagreeing, in either direction.
      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ifuSky.fold(
          ItcObservationDetails.AnalysisMethod.Aperture.Auto(
            skyAperture = 5.0
          )
        ): sky =>
          ifuAnalysis.getOrElse(GmosIfuAnalysis.Default) match
            case GmosIfuAnalysis.Sum(radius)    =>
              ItcObservationDetails.AnalysisMethod.Ifu.Sum(
                skyFibres = sky.fibres.value,
                num = Angle.signedDecimalArcseconds.get(radius).toDouble,
                // Ignored by the GMOS recipe, which reads `Gmos.isIfu2()` off the focal plane
                // unit instead, but sent faithfully rather than guessed.
                isIfu2 = isTwoSlitIfu
              )
            // `offset` is how far the IFU element sits from the source, not a sky aperture: a
            // non-zero value points the calculation away from the target and yields no signal.
            case GmosIfuAnalysis.Single(offset) =>
              ItcObservationDetails.AnalysisMethod.Ifu.Single(
                skyFibres = sky.fibres.value,
                offset = Angle.signedDecimalArcseconds.get(offset).toDouble
              )
    }

    case class GmosNorth(
      centralWavelength: Wavelength,
      disperser:         GmosNorthGrating,
      fpu:               GmosNorthFpuParam,
      filter:            Option[GmosNorthFilter],
      ccdMode:           Option[GmosCcdMode],
      roi:               Option[GmosRoi],
      portDisposition:   PortDisposition,
      ifuAnalysis:       Option[GmosIfuAnalysis]
    ) extends GmosSpectroscopy derives Hash {
      val ifuSky: Option[GmosIfuSky] = fpu.ifuSky
      val isTwoSlitIfu: Boolean      = fpu.isTwoSlitIfu

      val instrument: Instrument =
        Instrument.GmosNorth

      def resolution: Rational =
        disperser.resolution(centralWavelength, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(centralWavelength).toInterval)(
          (a, b) => a.intersect(b.coverageGN)
        )

      val description: String =
        if fpu.fpu.custom.isDefined then s"${instrument.shortName} MOS"
        else if fpu.isIfu then s"${instrument.shortName} IFU"
        else s"${instrument.shortName} Longslit"
    }

    case class GmosSouth(
      centralWavelength: Wavelength,
      disperser:         GmosSouthGrating,
      fpu:               GmosSouthFpuParam,
      filter:            Option[GmosSouthFilter],
      ccdMode:           Option[GmosCcdMode],
      roi:               Option[GmosRoi],
      portDisposition:   PortDisposition,
      ifuAnalysis:       Option[GmosIfuAnalysis]
    ) extends GmosSpectroscopy derives Hash {
      val ifuSky: Option[GmosIfuSky] = fpu.ifuSky
      val isTwoSlitIfu: Boolean      = fpu.isTwoSlitIfu

      val instrument: Instrument =
        Instrument.GmosSouth

      def resolution: Rational =
        disperser.resolution(centralWavelength, fpu.effectiveSlitWidth)

      def coverage: Interval[Wavelength] =
        filter.foldLeft(disperser.simultaneousCoverage.centeredAt(centralWavelength).toInterval)(
          (a, b) => a.intersect(b.coverageGS)
        )

      val description: String =
        if fpu.fpu.custom.isDefined then s"${instrument.shortName} MOS"
        else if fpu.isIfu then s"${instrument.shortName} IFU"
        else s"${instrument.shortName} Longslit"
    }

    case class Flamingos2(
      disperser:       Flamingos2Disperser,
      filter:          Flamingos2Filter,
      readMode:        Flamingos2ReadMode,
      fpu:             Flamingos2FpuMask,
      portDisposition: PortDisposition
    ) extends SpectroscopyMode derives Hash {

      override def analysisMethod: AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val instrument: Instrument =
        Instrument.Flamingos2

      val description: String =
        if fpu.custom.isDefined then s"${instrument.shortName} MOS"
        else s"${instrument.shortName} Longslit"
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
      stepCount:       PosInt,
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

    final case class GnirsSpectroscopy(
      centralWavelength: Wavelength,
      filter:            GnirsFilter,
      fpu:               GnirsFpu.Spectroscopy,
      prism:             GnirsPrism,
      grating:           GnirsGrating,
      camera:            GnirsCamera,
      readMode:          GnirsReadMode,
      wellDepth:         GnirsWellDepth,
      coadds:            PosInt,
      portDisposition:   PortDisposition
    ) extends SpectroscopyMode derives Hash {
      val instrument: Instrument =
        Instrument.Gnirs

      override def analysisMethod: AnalysisMethod =
        fpu match
          case GnirsFpu.Spectroscopy.Slit(_) =>
            ItcObservationDetails.AnalysisMethod.Aperture.Auto(
              skyAperture = 1.0
            )
          case GnirsFpu.Spectroscopy.Ifu(_)  =>
            // "Sum of 2x2 elements at the center" with a single sky fibre.
            ItcObservationDetails.AnalysisMethod.Ifu.Summed(
              skyFibres = 1,
              numX = 2,
              numY = 2,
              centerX = 0.0,
              centerY = 0.0
            )

      val description: String =
        fpu match
          case GnirsFpu.Spectroscopy.Slit(_) => s"${instrument.shortName} Longslit"
          case GnirsFpu.Spectroscopy.Ifu(_)  => s"${instrument.shortName} IFU"
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

    final case class Gnirs(
      filter:          GnirsFilter,
      camera:          GnirsCamera,
      readMode:        GnirsReadMode,
      wellDepth:       GnirsWellDepth,
      coadds:          PosInt,
      portDisposition: PortDisposition
    ) extends ImagingMode derives Hash {
      val instrument: Instrument = Instrument.Gnirs

      // Central wavelength is ignored for imaging; report the filter's.
      val centralWavelength: Wavelength = filter.centralWavelength

      def analysisMethod: ItcObservationDetails.AnalysisMethod =
        ItcObservationDetails.AnalysisMethod.Aperture.Auto(
          skyAperture = 1.0
        )

      val description: String =
        s"${instrument.shortName} Imaging"
    }
  }
}
