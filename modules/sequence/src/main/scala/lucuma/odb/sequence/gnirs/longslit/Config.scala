// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.longslit

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class AcquisitionConfig(
  explicitAcqType:  Option[GnirsAcquisitionType],
  coadds:           PosInt,
  filter:           GnirsFilter,
  skyOffset:        Option[Offset],
  exposureTimeMode: ExposureTimeMode,
  coadds:           PosInt,
):

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(128)
    val out = new DataOutputStream(bao)
    out.writeChars(explicitAcqType.fold("")(_.tag))
    out.write(coadds.value.hashBytes)
    out.writeChars(filter.tag)
    out.write(skyOffset.map(_.hashBytes).getOrElse(Array.emptyByteArray))
    out.write(exposureTimeMode.hashBytes)
    out.close()
    bao.toByteArray

object AcquisitionConfig:
  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (a.explicitAcqType, a.coadds.value, a.filter, a.skyOffset, a.exposureTimeMode)

case class Config(
  filter:                  GnirsFilter,
  decker:                  GnirsDecker,
  fpu:                     GnirsFpuSlit,
  prism:                   GnirsPrism,
  grating:                 GnirsGrating,
  gratingWavelength:       Wavelength,
  camera:                  GnirsCamera,
  focus:                   GnirsFocus,
  explicitReadMode:        Option[GnirsReadMode],
  wellDepth:               GnirsWellDepth,
  exposureTimeMode:        ExposureTimeMode,
  coadds:                  PosInt,
  telescopeConfigs:        SlitTelescopeConfigs,
  acquisition:             AcquisitionConfig
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(512)
    val out = new DataOutputStream(bao)

    out.writeChars(filter.tag)
    out.writeChars(decker.tag)
    out.writeChars(fpu.tag)
    out.writeChars(prism.tag)
    out.writeChars(grating.tag)
    out.write(gratingWavelength.hashBytes)
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

    out.close()
    bao.toByteArray
