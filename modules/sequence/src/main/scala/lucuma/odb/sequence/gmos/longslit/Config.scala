// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Eq
import cats.syntax.option.*
import cats.syntax.order.*
import coulomb.*
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosInt
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
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Picometer
import lucuma.core.math.units.Pixels
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.util.Enumerated
import monocle.Lens

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS Long Slit science mode.  Using these parameters, a
 * GMOS long slit sequence may be generated.
 * @tparam G grating type
 * @tparam L filter type
 * @tparam U FPU type
 */
sealed trait Config[G: Enumerated, L: Enumerated, U: Enumerated] extends Product with Serializable {
  def grating: G

  def coverage: WavelengthDelta

  def filter: Option[L]

  def fpu: U

  def centralWavelength: Wavelength


  def xBin: GmosXBinning =
    explicitXBin.getOrElse(defaultXBin)

  def defaultXBin: GmosXBinning

  def explicitXBin: Option[GmosXBinning]


  def yBin: GmosYBinning =
    explicitYBin.getOrElse(defaultYBin)

  def defaultYBin: GmosYBinning

  def explicitYBin: Option[GmosYBinning]


  def ampReadMode: GmosAmpReadMode =
    explicitAmpReadMode.getOrElse(defaultAmpReadMode)

  def defaultAmpReadMode: GmosAmpReadMode =
    DefaultAmpReadMode

  def explicitAmpReadMode: Option[GmosAmpReadMode]


  def ampGain: GmosAmpGain =
    explicitAmpGain.getOrElse(defaultAmpGain)

  def defaultAmpGain: GmosAmpGain =
    DefaultAmpGain

  def explicitAmpGain: Option[GmosAmpGain]


  def roi: GmosRoi =
    explicitRoi.getOrElse(defaultRoi)

  def defaultRoi: GmosRoi =
    DefaultRoi

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

  def ccdMode: GmosCcdMode =
    GmosCcdMode(
      xBin,
      yBin,
      DefaultAmpCount,
      ampGain,
      ampReadMode
    )

  def hashBytes: Array[Byte] = {
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(Enumerated[G].tag(grating))
    filter.foreach(f => out.writeChars(Enumerated[L].tag(f)))
    out.writeChars(Enumerated[U].tag(fpu))
    out.writeInt(centralWavelength.toPicometers.value.value)
    out.writeChars(xBin.tag)
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
    bao.toByteArray
  }

}

object Config {

  final case class GmosNorth private[longslit] (
    grating:                   GmosNorthGrating,
    filter:                    Option[GmosNorthFilter],
    fpu:                       GmosNorthFpu,
    centralWavelength:         Wavelength,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[List[WavelengthDither]],
    explicitSpatialOffsets:    Option[List[Q]]
  ) extends Config[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersNorth(this.grating)

  }

  object GmosNorth {

    def apply(
      sourceProfile:             SourceProfile,
      imageQuality:              ImageQuality.Preset,
      sampling:                  PosDouble,
      grating:                   GmosNorthGrating,
      filter:                    Option[GmosNorthFilter],
      fpu:                       GmosNorthFpu,
      centralWavelength:         Wavelength,
      explicitXBin:              Option[GmosXBinning]           = None,
      explicitYBin:              Option[GmosYBinning]           = None,
      explicitAmpReadMode:       Option[GmosAmpReadMode]        = None,
      explicitAmpGain:           Option[GmosAmpGain]            = None,
      explicitRoi:               Option[GmosRoi]                = None,
      explicitWavelengthDithers: Option[List[WavelengthDither]] = None,
      explicitSpatialOffsets:    Option[List[Q]]                = None
    ): GmosNorth = {
      val (x, y) = northBinning(fpu, sourceProfile, imageQuality.toImageQuality, grating, sampling = sampling)

      GmosNorth(
        grating,
        filter,
        fpu,
        centralWavelength,
        x,
        explicitXBin,
        y,
        explicitYBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi,
        explicitWavelengthDithers,
        explicitSpatialOffsets
      )
    }

    def reconcile(a: GmosNorth, modes: List[ObservingMode]): Option[GmosNorth] =
      modes.headOption match {
        case None                                                  =>
          a.some

        case Some(b: GmosNorth) =>
          if (a === b)
            reconcile(a, modes.tail)
          else {
            val x  = a.xBin min b.xBin
            val y  = a.yBin min b.yBin
            val aʹ = a.copy(explicitXBin = none, defaultXBin = x, explicitYBin = none, defaultYBin = y)
            val bʹ = b.copy(explicitXBin = none, defaultXBin = x, explicitYBin = none, defaultYBin = y)
            if (aʹ === bʹ) reconcile(aʹ, modes.tail) else none
          }

        case _                                                     =>
          none
      }


    given Eq[GmosNorth] =
      Eq.by { a => (
        a.grating,
        a.filter,
        a.fpu,
        a.centralWavelength,
        a.defaultXBin,
        a.explicitXBin,
        a.defaultYBin,
        a.explicitYBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi,
        a.explicitWavelengthDithers,
        a.explicitSpatialOffsets
      )}

  }

  final case class GmosSouth private[longslit] (
    grating:                   GmosSouthGrating,
    filter:                    Option[GmosSouthFilter],
    fpu:                       GmosSouthFpu,
    centralWavelength:         Wavelength,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[List[WavelengthDither]],
    explicitSpatialOffsets:    Option[List[Q]]
  ) extends Config[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersSouth(this.grating)

  }

  object GmosSouth {

    def apply(
      sourceProfile:             SourceProfile,
      imageQuality:              ImageQuality.Preset,
      sampling:                  PosDouble,
      grating:                   GmosSouthGrating,
      filter:                    Option[GmosSouthFilter],
      fpu:                       GmosSouthFpu,
      centralWavelength:         Wavelength,
      explicitXBin:              Option[GmosXBinning]           = None,
      explicitYBin:              Option[GmosYBinning]           = None,
      explicitAmpReadMode:       Option[GmosAmpReadMode]        = None,
      explicitAmpGain:           Option[GmosAmpGain]            = None,
      explicitRoi:               Option[GmosRoi]                = None,
      explicitWavelengthDithers: Option[List[WavelengthDither]] = None,
      explicitSpatialOffsets:    Option[List[Q]]                = None
    ): GmosSouth = {
      val (x, y) = southBinning(fpu, sourceProfile, imageQuality.toImageQuality, grating, sampling = sampling)

      GmosSouth(
        grating,
        filter,
        fpu,
        centralWavelength,
        x,
        explicitXBin,
        y,
        explicitYBin,
        explicitAmpReadMode,
        explicitAmpGain,
        explicitRoi,
        explicitWavelengthDithers,
        explicitSpatialOffsets
      )
    }

    def reconcile(a: GmosSouth, modes: List[ObservingMode]): Option[GmosSouth] =
      modes.headOption match {
        case None                                                  =>
          a.some

        case Some(b: GmosSouth) =>
          if (a === b)
            reconcile(a, modes.tail)
          else {
            val x  = a.xBin min b.xBin
            val y  = a.yBin min b.yBin
            val aʹ = a.copy(explicitXBin = none, defaultXBin = x, explicitYBin = none, defaultYBin = y)
            val bʹ = b.copy(explicitXBin = none, defaultXBin = x, explicitYBin = none, defaultYBin = y)
            if (aʹ === bʹ) reconcile(aʹ, modes.tail) else none
          }

        case _                                                     =>
          none
      }

    given Eq[GmosSouth] =
      Eq.by { a => (
        a.grating,
        a.filter,
        a.fpu,
        a.centralWavelength,
        a.defaultXBin,
        a.explicitXBin,
        a.defaultYBin,
        a.explicitYBin,
        a.explicitAmpReadMode,
        a.explicitAmpGain,
        a.explicitRoi,
        a.explicitWavelengthDithers,
        a.explicitSpatialOffsets
      )}

  }

  def explicitWavelengthDithers[G, L, U]: Lens[Config[G, L, U], Option[List[WavelengthDither]]] =
    Lens[Config[G, L, U], Option[List[WavelengthDither]]](_.explicitWavelengthDithers) { dithers => {
      case gn: GmosNorth => gn.copy(explicitWavelengthDithers = dithers)
      case gs: GmosSouth => gs.copy(explicitWavelengthDithers = dithers)
    }}

  def explicitSpatialOffsets[G, L, U]: Lens[Config[G, L, U], Option[List[Q]]] =
    Lens[Config[G, L, U], Option[List[Q]]](_.explicitSpatialOffsets) { qs => {
      case gn: GmosNorth => gn.copy(explicitSpatialOffsets = qs)
      case gs: GmosSouth => gs.copy(explicitSpatialOffsets = qs)
    }}

  val IfuSlitWidth: Angle =
    Angle.fromMicroarcseconds(310_000L)

  // ShortCut 3374
  val DefaultSpatialOffsets: List[Q] =
    List(
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(  0)),
      Q.signedDecimalArcseconds.reverseGet(BigDecimal( 15)),
      Q.signedDecimalArcseconds.reverseGet(BigDecimal(-15))
    )

  def gapSize(site: Site): Quantity[PosInt, Pixels] =
    site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.gapSize
      case Site.GS => GmosSouthDetector.Hamamatsu.gapSize
    }

  // wavelength dither needed to fill the chip gaps.
  private def defaultWavelengthDithers(ditherNm: Int): List[WavelengthDither] =
    List(
      WavelengthDither.Zero,
      WavelengthDither.decimalNanometers.getOption(BigDecimal( ditherNm)).get,
      WavelengthDither.decimalNanometers.getOption(BigDecimal(-ditherNm)).get
    )

  // ShortCut 3374
  def defaultWavelengthDithersNorth(grating: GmosNorthGrating): List[WavelengthDither] =
    defaultWavelengthDithers(grating match
      case GmosNorthGrating.B1200_G5301 |
           GmosNorthGrating.R831_G5302  |
           GmosNorthGrating.R600_G5304  |
           GmosNorthGrating.B480_G5309  => 5
      case GmosNorthGrating.R400_G5305  => 8
      case GmosNorthGrating.R150_G5308  => 20
    )

  // ShortCut 3374
  def defaultWavelengthDithersSouth(grating: GmosSouthGrating): List[WavelengthDither] =
    defaultWavelengthDithers(grating match
      case GmosSouthGrating.B1200_G5321 |
           GmosSouthGrating.R831_G5322  |
           GmosSouthGrating.B600_G5323  |
           GmosSouthGrating.R600_G5324  |
           GmosSouthGrating.B480_G5327  => 5
      case GmosSouthGrating.R400_G5325  => 8
      case GmosSouthGrating.R150_G5326  => 20
    )

}
