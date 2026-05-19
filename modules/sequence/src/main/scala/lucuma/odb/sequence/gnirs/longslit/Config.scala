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
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class AcquisitionConfig(
  readMode:      GnirsReadMode,
  coadds:        PosInt,
  filter:        GnirsFilter,
  offsetP:       Option[Offset.P],
  offsetQ:       Option[Offset.Q],
  exposureTime:  TimeSpan,
  exposureCount: PosInt,
  wavelength:    Wavelength
):

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(128)
    val out = new DataOutputStream(bao)
    out.writeChars(readMode.tag)
    out.write(coadds.value.hashBytes)
    out.writeChars(filter.tag)
    out.write(offsetP.map(_.hashBytes).getOrElse(Array.emptyByteArray))
    out.write(offsetQ.map(_.hashBytes).getOrElse(Array.emptyByteArray))
    out.write(exposureTime.hashBytes)
    out.write(exposureCount.value.hashBytes)
    out.write(wavelength.hashBytes)
    out.close()
    bao.toByteArray

object AcquisitionConfig:
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.readMode, a.coadds.value, a.filter, a.offsetP.map(_.toAngle), a.offsetQ.map(_.toAngle), a.exposureTime, a.exposureCount.value, a.wavelength)

case class Config(
  scienceExposureTimeMode: ExposureTimeMode,
  acquisitionMirrorMode:   GnirsAcquisitionMirrorMode,
  camera:                  GnirsCamera,
  fpu:                     GnirsFpuSlit,
  filter:                  GnirsFilter,
  centralWavelength:       Wavelength,
  coadds:                  PosInt,
  decker:                  GnirsDecker,
  readMode:                GnirsReadMode,
  wellDepth:               GnirsWellDepth,
  focus:                   GnirsFocus,
  telescopeConfigs:        SlitTelescopeConfigs,
  acquisition:             AcquisitionConfig
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(512)
    val out = new DataOutputStream(bao)

    out.write(scienceExposureTimeMode.hashBytes)

    // Acquisition mirror mode hash
    acquisitionMirrorMode match
      case GnirsAcquisitionMirrorMode.In =>
        out.writeChars("In")
      case GnirsAcquisitionMirrorMode.Out(prism, grating, wavelength) =>
        out.writeChars("Out")
        out.writeChars(prism.tag)
        out.writeChars(grating.tag)
        out.write(wavelength.value.hashBytes)

    out.writeChars(camera.tag)
    out.writeChars(fpu.tag)
    out.writeChars(filter.tag)
    out.write(centralWavelength.hashBytes)
    out.write(coadds.value.hashBytes)
    out.writeChars(decker.tag)
    out.writeChars(readMode.tag)
    out.writeChars(wellDepth.tag)
    focus match
      case GnirsFocus.Best          => out.writeByte(0)
      case GnirsFocus.Custom(qty)   =>
        out.writeByte(1)
        out.writeInt(qty.value.value.value)

    // Telescope configs hash
    telescopeConfigs.telescopeConfigs.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.write(acquisition.hashBytes)

    out.close()
    bao.toByteArray
