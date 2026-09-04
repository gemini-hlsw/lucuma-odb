// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.odb.sequence.gnirs.AcquisitionConfig
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/**
 * Configuration for the GNIRS imaging science mode. Imaging fixes the
 * FPU (acquisition), the decker (acquisition) and the acquisition
 * mirror (in), so none of them appears here.
 */
case class Config(
  variant:           Variant,
  filters:           NonEmptyList[Filter],
  camera:            GnirsCamera,
  explicitReadMode:  Option[GnirsReadMode],
  defaultWellDepth:  GnirsWellDepth,
  explicitWellDepth: Option[GnirsWellDepth],
  acquisition:       AcquisitionConfig
) derives Eq:

  def wellDepth: GnirsWellDepth =
    explicitWellDepth.getOrElse(defaultWellDepth)

  private lazy val coaddsByFilter: Map[GnirsFilter, PosInt] =
    filters.toList.map(f => f.filter -> f.coadds).toMap

  /**
   * Coadds for the given filter.  Falls back to 1 for a filter that isn't part
   * of this configuration, which the sequence never asks for.
   */
  def coaddsFor(filter: GnirsFilter): PosInt =
    coaddsByFilter.getOrElse(filter, PosInt.unsafeFrom(1))

  def staticConfig: GnirsStaticConfig =
    GnirsStaticConfig(wellDepth)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(variant.hashBytes)
    // Length-prefixed: the element count is user-controlled, so without it two
    // different filter lists could hash to the same bytes.
    out.writeInt(filters.length)
    filters.toList.foreach: f =>
      out.write(f.hashBytes)

    out.writeChars(camera.tag)
    out.writeChars(explicitReadMode.fold("")(_.tag))
    out.writeChars(wellDepth.tag)

    out.write(acquisition.hashBytes)

    out.close()
    bao.toByteArray
