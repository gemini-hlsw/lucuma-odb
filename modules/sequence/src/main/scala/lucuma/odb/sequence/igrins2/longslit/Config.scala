// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.igrins2.longslit

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class Config(
  scienceExposureTimeMode: ExposureTimeMode,
  offsetMode: Igrins2OffsetMode,
  saveSVCImages: Boolean,
  explicitSpatialOffsets: Option[List[Offset]]
) derives Eq:

  def offsets: List[Offset] =
    explicitSpatialOffsets.getOrElse(Config.defaultOffsetsFor(offsetMode))

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(256)
    val out = new DataOutputStream(bao)

    out.write(scienceExposureTimeMode.hashBytes)
    out.writeChars(offsetMode.tag)
    out.writeBoolean(saveSVCImages)
    val off = explicitSpatialOffsets.foldMap(_.map(_.hashBytes)).flatten.toArray
    out.write(off, 0, off.length)

    out.close()
    bao.toByteArray

object Config:
  // TODO: Move this logic to lucuma-core

  val NodAlongSlitDefaultOffsets: List[Offset] =
    List(
      Offset.Zero.copy(q = -1.25.qArcsec),
      Offset.Zero.copy(q =  1.25.qArcsec),
      Offset.Zero.copy(q =  1.25.qArcsec),
      Offset.Zero.copy(q = -1.25.qArcsec),
    )

  val DefaultSpatialOffsets: List[Offset] = NodAlongSlitDefaultOffsets

  val NodToSkyDefaultOffsets: List[Offset] =
    List(
      Offset.Zero,
      Offset.Zero.copy(p = 10.0.pArcsec, q = 10.0.qArcsec),
      Offset.Zero,
    )

  def defaultOffsetsFor(mode: Igrins2OffsetMode): List[Offset] =
    mode match
      case Igrins2OffsetMode.NodAlongSlit => NodAlongSlitDefaultOffsets
      case Igrins2OffsetMode.NodToSky     => NodToSkyDefaultOffsets
