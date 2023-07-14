// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import cats.Eq
import cats.Order
import cats.syntax.order.*
import coulomb.*
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosInt
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.security.MessageDigest
import java.util.HexFormat
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.math.units.Picometer
import lucuma.core.math.units.Pixels
import lucuma.core.model.SourceProfile
import lucuma.core.syntax.enumerated.*
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Configuration for the GMOS Long Slit science mode.  Using these parameters, a
 * GMOS long slit sequence may be generated.
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait Config[G: Enumerated, F: Enumerated, U: Enumerated] extends Product with Serializable {
  def grating: G

  def filter: Option[F]

  def fpu: U

  def centralWavelength: Wavelength


  def xBin(
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): GmosXBinning =
    explicitXBin.getOrElse(defaultXBin(sourceProfile, imageQuality, sampling))

  def defaultXBin(
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): GmosXBinning

  def explicitXBin: Option[GmosXBinning]


  def yBin: GmosYBinning =
    explicitYBin.getOrElse(defaultYBin)

  def defaultYBin: GmosYBinning =
    Config.DefaultYBinning

  def explicitYBin: Option[GmosYBinning]


  def ampReadMode: GmosAmpReadMode =
    explicitAmpReadMode.getOrElse(defaultAmpReadMode)

  def defaultAmpReadMode: GmosAmpReadMode =
    Config.DefaultAmpReadMode

  def explicitAmpReadMode: Option[GmosAmpReadMode]


  def ampGain: GmosAmpGain =
    explicitAmpGain.getOrElse(defaultAmpGain)

  def defaultAmpGain: GmosAmpGain =
    Config.DefaultAmpGain

  def explicitAmpGain: Option[GmosAmpGain]


  def roi: GmosRoi =
    explicitRoi.getOrElse(defaultRoi)

  def defaultRoi: GmosRoi =
    Config.DefaultRoi

  def explicitRoi: Option[GmosRoi]


  def wavelengthDithers: List[WavelengthDither] =
    explicitWavelengthDithers.getOrElse(defaultWavelengthDithers)

  def defaultWavelengthDithers: List[WavelengthDither]

  def explicitWavelengthDithers: Option[List[WavelengthDither]]


  def spatialOffsets: List[Q] =
    explicitSpatialOffsets.getOrElse(defaultSpatialOffsets)

  def defaultSpatialOffsets: List[Q] =
    Config.DefaultSpatialOffsets

  def explicitSpatialOffsets: Option[List[Q]]

  def hash(
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): String = {

    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(grating.tag)
    filter.foreach(f => out.writeChars(f.tag))
    out.writeChars(fpu.tag)
    out.writeInt(centralWavelength.toPicometers.value.value)
    out.writeChars(xBin(sourceProfile, imageQuality, sampling).tag)
    out.writeChars(yBin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)
    wavelengthDithers.foreach { d =>
      out.writeInt(d.toPicometers.value)
    }
    spatialOffsets.foreach { o =>
      out.writeLong(o.toAngle.toMicroarcseconds)
    }

    out.close()

    Config.hex.formatHex(
      MessageDigest
        .getInstance("MD5")
        .digest(bao.toByteArray)
    )
  }

}

object Config {

  private val hex = HexFormat.of

  final case class GmosNorth(
    grating:             GmosNorthGrating,
    filter:              Option[GmosNorthFilter],
    fpu:                 GmosNorthFpu,
    centralWavelength:   Wavelength,
    explicitXBin:        Option[GmosXBinning]    = None,
    explicitYBin:        Option[GmosYBinning]    = None,
    explicitAmpReadMode: Option[GmosAmpReadMode] = None,
    explicitAmpGain:     Option[GmosAmpGain]     = None,
    explicitRoi:         Option[GmosRoi]         = None,
    explicitWavelengthDithers: Option[List[WavelengthDither]] = None,
    explicitSpatialOffsets:    Option[List[Q]]                = None
  ) extends Config[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def defaultXBin(
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): GmosXBinning =
      xbinNorth(fpu, sourceProfile, imageQuality, sampling)

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersNorth(this.grating)

  }

  object GmosNorth {

    given Eq[GmosNorth] =
      Eq.by { a => (
        a.grating,
        a.filter,
        a.fpu,
        a.centralWavelength,
        a.explicitXBin,
        a.explicitYBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi,
        a.explicitWavelengthDithers,
        a.explicitSpatialOffsets
      )}

  }

  final case class GmosSouth(
    grating:             GmosSouthGrating,
    filter:              Option[GmosSouthFilter],
    fpu:                 GmosSouthFpu,
    centralWavelength:   Wavelength,
    explicitXBin:        Option[GmosXBinning]    = None,
    explicitYBin:        Option[GmosYBinning]    = None,
    explicitAmpReadMode: Option[GmosAmpReadMode] = None,
    explicitAmpGain:     Option[GmosAmpGain]     = None,
    explicitRoi:         Option[GmosRoi]         = None,
    explicitWavelengthDithers: Option[List[WavelengthDither]] = None,
    explicitSpatialOffsets:    Option[List[Q]]                = None
  ) extends Config[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def defaultXBin(
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): GmosXBinning =
      xbinSouth(fpu, sourceProfile, imageQuality, sampling)

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersSouth(this.grating)

  }

  object GmosSouth {

    given Eq[GmosSouth] =
      Eq.by { a => (
        a.grating,
        a.filter,
        a.fpu,
        a.centralWavelength,
        a.explicitXBin,
        a.explicitYBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi,
        a.explicitWavelengthDithers,
        a.explicitSpatialOffsets
      )}

  }

  val DefaultAmpReadMode: GmosAmpReadMode =
    GmosAmpReadMode.Slow

  val DefaultAmpGain: GmosAmpGain =
    GmosAmpGain.Low

  val DefaultRoi: GmosRoi =
    GmosRoi.FullFrame

  val DefaultYBinning: GmosYBinning =
    GmosYBinning.Two

  private implicit val AngleOrder: Order[Angle] =
    Angle.AngleOrder

  val IfuSlitWidth: Angle =
    Angle.fromMicroarcseconds(310_000L)

  val DefaultSpatialOffsets: List[Q] =
    List(
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(0)),
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(15)),
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(15)),
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(0))
    )

  /**
   * Object angular size estimate based on source profile alone.
   */
  def objectSize(p: SourceProfile): Angle =
    p match {
      case SourceProfile.Point(_)          => Angle.Angle0
      case SourceProfile.Uniform(_)        => Angle.Angle180
      case SourceProfile.Gaussian(fwhm, _) => fwhm
    }

  /**
   * Effective size of a target with the given source profile and image quality.
   */
  def effectiveSize(p: SourceProfile, iq: ImageQuality): Angle =
    objectSize(p) max iq.toAngle

  def effectiveSlitWidth(p: SourceProfile, iq: ImageQuality, slitWidth: Angle): Angle =
    slitWidth min effectiveSize(p, iq)

  def pixelSize(site: Site): Angle =
    site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.pixelSize
      case Site.GS => GmosSouthDetector.Hamamatsu.pixelSize
    }

  def gapSize(site: Site): Quantity[PosInt, Pixels] =
    site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.gapSize
      case Site.GS => GmosSouthDetector.Hamamatsu.gapSize
    }

  private val DescendingXBinning: List[GmosXBinning] =
    GmosXBinning.all.sortBy(b => -b.count)

  private def xbin(site: Site, slitWidth: Angle, sampling: PosDouble): GmosXBinning = {
    val npix = slitWidth.toMicroarcseconds.toDouble / pixelSize(site).toMicroarcseconds.toDouble
    DescendingXBinning.find(b => npix / b.count.toDouble >= sampling.value).getOrElse(GmosXBinning.One)
  }

  /**
   * Calculates the best `GmosXBinning` value to use for GMOS North long slit observing for
   * the desired sampling.
   *
   * @param fpu      GMOS North FPU
   * @param p        SourceProfile of the target
   * @param iq       expected/required ImageQuality
   * @param sampling desired sampling rate
   */
  def xbinNorth(fpu: GmosNorthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
    xbin(Site.GN, fpu.effectiveSlitWidth min effectiveSize(p, iq), sampling)

  /**
   * Calculates the best `GmosXBinning` value to use for GMOS South long slit observing for
   * the desired sampling.
   *
   * @param fpu      GMOS South FPU
   * @param p        SourceProfile of the target
   * @param iq       expected/required ImageQuality
   * @param sampling desired sampling rate
   */
  def xbinSouth(fpu: GmosSouthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
    xbin(Site.GS, fpu.effectiveSlitWidth min effectiveSize(p, iq), sampling)

  /**
   * Calculates the wavelength offsets required to fill in the chip gaps,
   * rounded to the nearest 5 nm.
   *
   * @param dispersion - dispersion in nm/pix (see https://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings)
   */
  def Δλ(site: Site, dispersion: Quantity[Rational, NanometersPerPixel]): WavelengthDither = {
    val d = dispersion.value.toDouble
    val g = gapSize(site).value.value
    val v = d * g * 2.0 // raw value, which we round to nearest 5 nm
    WavelengthDither.picometers.get(
      Quantity[Picometer](((v / 5.0).round * 5.0).toInt * 1000)
    )
  }
  def defaultWavelengthDithersNorth(grating: GmosNorthGrating): List[WavelengthDither] = {
    val delta = Δλ(Site.GN, grating.dispersion)
    List(WavelengthDither.Zero, delta, delta, WavelengthDither.Zero)
  }

  def defaultWavelengthDithersSouth(grating: GmosSouthGrating): List[WavelengthDither] = {
    val delta = Δλ(Site.GS, grating.dispersion)
    List(WavelengthDither.Zero, delta, delta, WavelengthDither.Zero)
  }

}
