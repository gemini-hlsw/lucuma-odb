// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.igrins2.longslit

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.ExposureTimeMode

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class Config(
  scienceExposureTimeMode: ExposureTimeMode,
  offsetMode: Igrins2OffsetMode,
  saveSVCImages: Boolean
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(scienceExposureTimeMode.hashBytes)
    out.writeChars(offsetMode.tag)
    out.writeBoolean(saveSVCImages)

    out.close()
    bao.toByteArray
