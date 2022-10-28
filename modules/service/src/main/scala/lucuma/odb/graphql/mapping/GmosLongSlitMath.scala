// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Order
import cats.syntax.order.*
import coulomb.*
import coulomb.units.accepted.ArcSecond
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.math.units.Pixels
import lucuma.core.model.SourceProfile
import spire.math.Rational

object GmosLongSlitMath {

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

  val zeroNm: Quantity[BigDecimal, Nanometer] =
    Quantity[Nanometer](BigDecimal(0))
    
  def defaultWavelengthDithersNorth(grating: GmosNorthGrating): List[Quantity[BigDecimal, Nanometer]] = {
    val deltaNm = Δλ(Site.GN, grating.dispersion)
    List(zeroNm, deltaNm, deltaNm, zeroNm)
  }

  def defaultWavelengthDithersSouth(grating: GmosSouthGrating): List[Quantity[BigDecimal, Nanometer]] = {
    val deltaNm = Δλ(Site.GS, grating.dispersion)
    List(zeroNm, deltaNm, deltaNm, zeroNm)
  }

  val DefaultSpatialOffsets: List[Quantity[BigDecimal, ArcSecond]] =
    List(
      Quantity[ArcSecond](BigDecimal(0)),
      Quantity[ArcSecond](BigDecimal(15)),
      Quantity[ArcSecond](BigDecimal(15)),
      Quantity[ArcSecond](BigDecimal(0))
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
  def Δλ(site: Site, dispersion: Quantity[Rational, NanometersPerPixel]): Quantity[BigDecimal, Nanometer] = {
    val d = dispersion.value.toDouble
    val g = gapSize(site).value.value
    val v = d * g * 2.0 // raw value, which we round to nearest 5 nm
    Quantity[Nanometer](BigDecimal(((v / 5.0).round * 5.0).toInt))
  }

}
