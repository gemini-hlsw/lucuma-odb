// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.longslit

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class AcquisitionConfig(
  explicitAcqMode:  Option[GnirsAcquisitionMode], // None => AUTO (from integration time); Faint carries its sky offset
  explicitFilter:   Option[GnirsFilter],          // None => AUTO (H2 for VeryBright, from wavelength for Bright/Faint)
  exposureTimeMode: ExposureTimeMode,
  coadds:           PosInt,
):

  /** The acquisition mode: the explicit choice if set, else the default for the integration time. */
  def resolvedMode(acqExposureTime: TimeSpan): GnirsAcquisitionMode =
    explicitAcqMode.getOrElse(GnirsAcquisitionMode.defaultFor(acqExposureTime, coadds))

  /**
   * The selected acquisition filter: the explicit filter if set, otherwise the
   * automatic choice — H2 for VeryBright (low transmission), or the filter for the
   * spectroscopy wavelength for Bright/Faint. This is the filter used for all
   * acquisition steps except the VeryBright FPU image, which always uses H (Order4).
   */
  def selectedFilter(mode: GnirsAcquisitionMode, wavelength: Wavelength): Either[String, GnirsFilter] =
    explicitFilter match
      case Some(f) => Right(f)
      case None    =>
        mode match
          case GnirsAcquisitionMode.VeryBright => Right(GnirsFilter.H2)
          case _                               => GnirsFilter.fromSpectroscopyWavelength(wavelength, isAcquisition = true)

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(128)
    val out = new DataOutputStream(bao)
    // explicit acquisition mode (None => AUTO): tag byte + offset (Faint only)
    explicitAcqMode match
      case None                                  => out.writeByte(0)
      case Some(GnirsAcquisitionMode.VeryBright) => out.writeByte(1)
      case Some(GnirsAcquisitionMode.Bright)     => out.writeByte(2)
      case Some(GnirsAcquisitionMode.Faint(o))   =>
        out.writeByte(3)
        out.write(o.hashBytes)
    out.writeChars(explicitFilter.fold("")(_.tag))
    out.write(coadds.value.hashBytes)
    out.write(exposureTimeMode.hashBytes)
    out.close()
    bao.toByteArray

object AcquisitionConfig:
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.explicitAcqMode, a.explicitFilter, a.coadds.value, a.exposureTimeMode)

case class Config(
  filter:                  GnirsFilter,
  decker:                  GnirsDecker,
  fpu:                     GnirsFpuSlit,
  prism:                   GnirsPrism,
  grating:                 GnirsGrating,
  centralWavelength:       Wavelength,
  camera:                  GnirsCamera,
  focus:                   GnirsFocus,
  explicitReadMode:        Option[GnirsReadMode],
  wellDepth:               GnirsWellDepth,
  exposureTimeMode:        ExposureTimeMode,
  coadds:                  PosInt,
  telescopeConfigs:        SlitTelescopeConfigs,
  acquisition:             AcquisitionConfig,
  telluricType:            TelluricType
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(512)
    val out = new DataOutputStream(bao)

    out.writeChars(filter.tag)
    out.writeChars(decker.tag)
    out.writeChars(fpu.tag)
    out.writeChars(prism.tag)
    out.writeChars(grating.tag)
    out.write(centralWavelength.hashBytes)
    out.writeChars(camera.tag)
    focus match
      case GnirsFocus.Best          => out.writeByte(0)
      case GnirsFocus.Custom(qty)   =>
        out.writeByte(1)
        out.writeInt(qty.value.value.value)
    out.writeChars(explicitReadMode.fold("")(_.tag))
    out.writeChars(wellDepth.tag)
    out.write(exposureTimeMode.hashBytes)
    out.write(coadds.value.hashBytes)

    telescopeConfigs.telescopeConfigs.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.write(acquisition.hashBytes)
    out.write(telluricType.hashBytes)

    out.close()
    bao.toByteArray
