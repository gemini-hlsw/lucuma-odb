// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.spectroscopy

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.sequence.gnirs.AcquisitionConfig
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class Config(
  filter:                  GnirsFilter,
  decker:                  GnirsDecker,
  fpu:                     GnirsFpu.Spectroscopy,
  prism:                   GnirsPrism,
  grating:                 GnirsGrating,
  centralWavelength:       Wavelength,
  camera:                  GnirsCamera,
  focus:                   GnirsFocus,
  explicitReadMode:        Option[GnirsReadMode],
  wellDepth:               GnirsWellDepth,
  exposureTimeMode:        ExposureTimeMode,
  coadds:                  PosInt,
  telescopeConfigs:        NonEmptyList[TelescopeConfig],
  acquisition:             AcquisitionConfig,
  telluricType:            TelluricType
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(512)
    val out = new DataOutputStream(bao)

    out.writeChars(filter.tag)
    out.writeChars(decker.tag)
    // FPU: discriminator byte + leaf tag, so slit and IFU tag namespaces can't collide.
    fpu match
      case GnirsFpu.Spectroscopy.Slit(s) =>
        out.writeByte(0)
        out.writeChars(s.tag)
      case GnirsFpu.Spectroscopy.Ifu(i)  =>
        out.writeByte(1)
        out.writeChars(i.tag)
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

    telescopeConfigs.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.write(acquisition.hashBytes)
    out.write(telluricType.hashBytes)

    out.close()
    bao.toByteArray
