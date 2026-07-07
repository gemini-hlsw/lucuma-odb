// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GNIRS imaging science mode. Keyhole imaging fixes the
 * FPU (acquisition keyhole), the decker (acquisition) and the acquisition
 * mirror (in), so none of them appears here.
 */
case class Config(
  variant:           Variant,
  filters:           NonEmptyList[Filter],
  camera:            GnirsCamera,
  coadds:            PosInt,
  explicitReadMode:  Option[GnirsReadMode],
  defaultWellDepth:  GnirsWellDepth,
  explicitWellDepth: Option[GnirsWellDepth]
) derives Eq:

  def wellDepth: GnirsWellDepth =
    explicitWellDepth.getOrElse(defaultWellDepth)

  def staticConfig: GnirsStaticConfig =
    GnirsStaticConfig(wellDepth)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(variant.hashBytes)
    out.write(filters.hashBytes)

    out.writeChars(camera.tag)
    out.write(coadds.value.hashBytes)
    out.writeChars(explicitReadMode.fold("")(_.tag))
    out.writeChars(wellDepth.tag)

    out.close()
    bao.toByteArray
