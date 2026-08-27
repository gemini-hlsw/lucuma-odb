// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Eq
import cats.data.NonEmptyList
import coulomb.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Pixels
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.util.Enumerated
import lucuma.odb.sequence.gmos.spectroscopy
import lucuma.odb.sequence.gmos.spectroscopy.Config.Common
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GMOS Long Slit science mode.  Using these parameters, a
 * GMOS long slit sequence may be generated.
 * @tparam G grating type
 * @tparam L filter type
 * @tparam U FPU type
 */
sealed trait Config[G: Enumerated, L: Enumerated, U: Enumerated] extends spectroscopy.Config[G, L, U]:

  def fpu: U

  override def fpuMask: GmosFpuMask[U] =
    GmosFpuMask.Builtin(fpu)

  override def gcalFpu: U =
    fpu

  def acquisition: AcquisitionConfig[L]

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(Enumerated[G].tag(grating))
    filter.foreach(f => out.writeChars(Enumerated[L].tag(f)))
    out.writeChars(Enumerated[U].tag(fpu))
    out.writeInt(centralWavelength.toPicometers.value.value)
    out.write(exposureTimeMode.hashBytes)
    out.writeChars(xBin.tag)
    out.writeChars(yBin.tag)
    out.writeChars(ampGain.tag)
    out.writeChars(ampReadMode.tag)
    out.writeChars(roi.tag)
    wavelengthDithers.foreach: d =>
      out.writeInt(d.toPicometers.value)
    telescopeConfigs.toList.foreach: tc =>
      out.writeLong(tc.offset.p.toAngle.toMicroarcseconds)
      out.writeLong(tc.offset.q.toAngle.toMicroarcseconds)
      out.writeChars(tc.guiding.tag)
    out.write(acquisition.hashBytes)

    out.close()
    bao.toByteArray

object Config:

  final case class GmosNorth(
    grating:          GmosNorthGrating,
    filter:           Option[GmosNorthFilter],
    fpu:              GmosNorthFpu,
    common:           Common,
    telescopeConfigs: NonEmptyList[TelescopeConfig],
    acquisition:      AcquisitionConfig.GmosNorth
  ) extends Config[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersNorth(this.grating)

    override def withWavelengthDithers(dithers: Option[List[WavelengthDither]]): GmosNorth =
      copy(common = common.copy(explicitWavelengthDithers = dithers))

    override def withTelescopeConfigs(tcs: NonEmptyList[TelescopeConfig]): GmosNorth =
      copy(telescopeConfigs = tcs)

  object GmosNorth:

    given Eq[GmosNorth] =
      Eq.by: a =>
        (
          a.grating,
          a.filter,
          a.fpu,
          a.common,
          a.telescopeConfigs,
          a.acquisition
        )

  final case class GmosSouth(
    grating:          GmosSouthGrating,
    filter:           Option[GmosSouthFilter],
    fpu:              GmosSouthFpu,
    common:           Common,
    telescopeConfigs: NonEmptyList[TelescopeConfig],
    acquisition:      AcquisitionConfig.GmosSouth
  ) extends Config[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:

    override def coverage: WavelengthDelta =
      grating.simultaneousCoverage

    override def defaultWavelengthDithers: List[WavelengthDither] =
      defaultWavelengthDithersSouth(this.grating)

    override def withWavelengthDithers(dithers: Option[List[WavelengthDither]]): GmosSouth =
      copy(common = common.copy(explicitWavelengthDithers = dithers))

    override def withTelescopeConfigs(tcs: NonEmptyList[TelescopeConfig]): GmosSouth =
      copy(telescopeConfigs = tcs)

  object GmosSouth:

    given Eq[GmosSouth] =
      Eq.by: a =>
        (
          a.grating,
          a.filter,
          a.fpu,
          a.common,
          a.telescopeConfigs,
          a.acquisition
        )

  val IfuSlitWidth: Angle =
    Angle.fromMicroarcseconds(310_000L)

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

  // ShortCut 3374, 6778
  def defaultWavelengthDithersNorth(grating: GmosNorthGrating): List[WavelengthDither] =
    defaultWavelengthDithers(grating match
      case GmosNorthGrating.B1200_G5301 |
           GmosNorthGrating.R831_G5302  |
           GmosNorthGrating.R600_G5304  => 5
      case GmosNorthGrating.R400_G5310  |
           GmosNorthGrating.B480_G5309  => 8
      case GmosNorthGrating.R150_G5308  => 20
    )

  // ShortCut 3374, 6778
  def defaultWavelengthDithersSouth(grating: GmosSouthGrating): List[WavelengthDither] =
    defaultWavelengthDithers(grating match
      case GmosSouthGrating.B1200_G5321 |
           GmosSouthGrating.R831_G5322  |
           GmosSouthGrating.R600_G5324  => 5
      case GmosSouthGrating.R400_G5325  |
           GmosSouthGrating.B480_G5327  => 8
      case GmosSouthGrating.R150_G5326  => 20
    )
