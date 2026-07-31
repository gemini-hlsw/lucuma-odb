// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.mos

import cats.Eq
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Defined
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.gmos.longslit.Config as LongSlitConfig
import lucuma.odb.sequence.syntax.hash.*
import monocle.Lens

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS MOS science mode.
 *
 * This is GMOS long slit with the builtin FPU replaced by a custom mask and
 * with no acquisition. Sequence generation for MOS is not implemented, so the
 * configuration is currently only read back, used for the ITC, and used to
 * derive the shared long slit calibration.
 *
 * @tparam G grating type
 * @tparam L filter type
 * @tparam U the builtin FPU type the custom mask's slit width corresponds to
 */
sealed trait Config[G: Enumerated, L: Enumerated, U] extends Product with Serializable:
  def grating: G

  def coverage: WavelengthDelta

  def filter: Option[L]

  def customMask: GmosFpuMask.Custom

  /**
   * The builtin long slit FPU whose aperture matches the custom mask's slit
   * width. GmosCustomSlitWidth and the builtin long slit FPUs are in 1:1
   * correspondence, which is what lets a MOS observation be calibrated, binned
   * and estimated as a long slit.
   */
  def equivalentFpu: U

  def centralWavelength: Wavelength

  def exposureTimeMode: ExposureTimeMode

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
    LongSlitConfig.DefaultSpatialOffsets

  def explicitSpatialOffsets: Option[List[Q]]

  def ccdMode: GmosCcdMode =
    GmosCcdMode(
      xBin,
      yBin,
      DefaultAmpCount,
      ampGain,
      ampReadMode
    )

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(Enumerated[G].tag(grating))
    filter.foreach(f => out.writeChars(Enumerated[L].tag(f)))
    out.writeChars(customMask.slitWidth.tag)
    customMask.mask match
      case ToBeDefined => ()
      case Defined(id) => out.writeLong(id.value.value)
    out.writeInt(centralWavelength.toPicometers.value.value)
    out.write(exposureTimeMode.hashBytes)
    out.writeChars(xBin.tag)
    out.writeChars(yBin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)
    wavelengthDithers.foreach: d =>
      out.writeInt(d.toPicometers.value)
    spatialOffsets.foreach: o =>
      out.writeLong(o.toAngle.toMicroarcseconds)

    out.close()
    bao.toByteArray

object Config:

  final case class Common(
    centralWavelength:         Wavelength,
    exposureTimeMode:          ExposureTimeMode,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[List[WavelengthDither]],
    explicitSpatialOffsets:    Option[List[Q]]
  )

  object Common:

    given Eq[Common] =
      Eq.by: a =>
        (
          a.centralWavelength,
          a.exposureTimeMode,
          a.defaultXBin,
          a.explicitXBin,
          a.defaultYBin,
          a.explicitYBin,
          a.explicitAmpReadMode,
          a.explicitAmpGain,
          a.explicitRoi,
          a.explicitWavelengthDithers,
          a.explicitSpatialOffsets
        )

  final case class GmosNorth(
    grating:    GmosNorthGrating,
    filter:     Option[GmosNorthFilter],
    customMask: GmosFpuMask.Custom,
    common:     Common
  ) extends Config[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def equivalentFpu: GmosNorthFpu =
      Config.northFpu(customMask.slitWidth)

    override def centralWavelength: Wavelength =
      common.centralWavelength

    override def exposureTimeMode: ExposureTimeMode =
      common.exposureTimeMode

    override def defaultXBin: GmosXBinning =
      common.defaultXBin

    override def explicitXBin: Option[GmosXBinning] =
      common.explicitXBin

    override def defaultYBin: GmosYBinning =
      common.defaultYBin

    override def explicitYBin: Option[GmosYBinning] =
      common.explicitYBin

    override def explicitAmpReadMode: Option[GmosAmpReadMode] =
      common.explicitAmpReadMode

    override def explicitAmpGain: Option[GmosAmpGain] =
      common.explicitAmpGain

    override def explicitRoi: Option[GmosRoi] =
      common.explicitRoi

    override def defaultWavelengthDithers: List[WavelengthDither] =
      LongSlitConfig.defaultWavelengthDithersNorth(this.grating)

    override def explicitWavelengthDithers: Option[List[WavelengthDither]] =
      common.explicitWavelengthDithers

    override def explicitSpatialOffsets: Option[List[Q]] =
      common.explicitSpatialOffsets

  object GmosNorth:

    given Eq[GmosNorth] =
      Eq.by: a =>
        (
          a.grating,
          a.filter,
          a.customMask,
          a.common
        )

  final case class GmosSouth(
    grating:    GmosSouthGrating,
    filter:     Option[GmosSouthFilter],
    customMask: GmosFpuMask.Custom,
    common:     Common
  ) extends Config[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def equivalentFpu: GmosSouthFpu =
      Config.southFpu(customMask.slitWidth)

    override def centralWavelength: Wavelength =
      common.centralWavelength

    override def exposureTimeMode: ExposureTimeMode =
      common.exposureTimeMode

    override def defaultXBin: GmosXBinning =
      common.defaultXBin

    override def explicitXBin: Option[GmosXBinning] =
      common.explicitXBin

    override def defaultYBin: GmosYBinning =
      common.defaultYBin

    override def explicitYBin: Option[GmosYBinning] =
      common.explicitYBin

    override def explicitAmpReadMode: Option[GmosAmpReadMode] =
      common.explicitAmpReadMode

    override def explicitAmpGain: Option[GmosAmpGain] =
      common.explicitAmpGain

    override def explicitRoi: Option[GmosRoi] =
      common.explicitRoi

    override def defaultWavelengthDithers: List[WavelengthDither] =
      LongSlitConfig.defaultWavelengthDithersSouth(this.grating)

    override def explicitWavelengthDithers: Option[List[WavelengthDither]] =
      common.explicitWavelengthDithers

    override def explicitSpatialOffsets: Option[List[Q]] =
      common.explicitSpatialOffsets

  object GmosSouth:

    given Eq[GmosSouth] =
      Eq.by: a =>
        (
          a.grating,
          a.filter,
          a.customMask,
          a.common
        )

  def explicitWavelengthDithers[G, L, U]: Lens[Config[G, L, U], Option[List[WavelengthDither]]] =
    Lens[Config[G, L, U], Option[List[WavelengthDither]]](_.explicitWavelengthDithers) { dithers => {
      case gn: GmosNorth => gn.copy(common = gn.common.copy(explicitWavelengthDithers = dithers))
      case gs: GmosSouth => gs.copy(common = gs.common.copy(explicitWavelengthDithers = dithers))
    }}

  def explicitSpatialOffsets[G, L, U]: Lens[Config[G, L, U], Option[List[Q]]] =
    Lens[Config[G, L, U], Option[List[Q]]](_.explicitSpatialOffsets) { qs => {
      case gn: GmosNorth => gn.copy(common = gn.common.copy(explicitSpatialOffsets = qs))
      case gs: GmosSouth => gs.copy(common = gs.common.copy(explicitSpatialOffsets = qs))
    }}

  def northFpu(slitWidth: GmosCustomSlitWidth): GmosNorthFpu =
    slitWidth match
      case GmosCustomSlitWidth.CustomWidth_0_25 => GmosNorthFpu.LongSlit_0_25
      case GmosCustomSlitWidth.CustomWidth_0_50 => GmosNorthFpu.LongSlit_0_50
      case GmosCustomSlitWidth.CustomWidth_0_75 => GmosNorthFpu.LongSlit_0_75
      case GmosCustomSlitWidth.CustomWidth_1_00 => GmosNorthFpu.LongSlit_1_00
      case GmosCustomSlitWidth.CustomWidth_1_50 => GmosNorthFpu.LongSlit_1_50
      case GmosCustomSlitWidth.CustomWidth_2_00 => GmosNorthFpu.LongSlit_2_00
      case GmosCustomSlitWidth.CustomWidth_5_00 => GmosNorthFpu.LongSlit_5_00

  def southFpu(slitWidth: GmosCustomSlitWidth): GmosSouthFpu =
    slitWidth match
      case GmosCustomSlitWidth.CustomWidth_0_25 => GmosSouthFpu.LongSlit_0_25
      case GmosCustomSlitWidth.CustomWidth_0_50 => GmosSouthFpu.LongSlit_0_50
      case GmosCustomSlitWidth.CustomWidth_0_75 => GmosSouthFpu.LongSlit_0_75
      case GmosCustomSlitWidth.CustomWidth_1_00 => GmosSouthFpu.LongSlit_1_00
      case GmosCustomSlitWidth.CustomWidth_1_50 => GmosSouthFpu.LongSlit_1_50
      case GmosCustomSlitWidth.CustomWidth_2_00 => GmosSouthFpu.LongSlit_2_00
      case GmosCustomSlitWidth.CustomWidth_5_00 => GmosSouthFpu.LongSlit_5_00
