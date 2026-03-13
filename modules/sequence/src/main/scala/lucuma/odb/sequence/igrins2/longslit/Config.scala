// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.igrins2.longslit

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.igrins2.*
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
    explicitSpatialOffsets.getOrElse(defaultOffsetsFor(offsetMode))

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
