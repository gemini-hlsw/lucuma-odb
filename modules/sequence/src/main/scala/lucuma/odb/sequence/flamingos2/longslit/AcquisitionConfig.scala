// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.longslit

import cats.Eq
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.sequence.syntax.hash.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream


final case class AcquisitionConfig(
  exposureTimeMode: ExposureTimeMode,
  defaultFilter:    Flamingos2Filter,
  explicitFilter:   Option[Flamingos2Filter]
):

  def filter: Flamingos2Filter =
    explicitFilter.getOrElse(defaultFilter)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(filter.tag)
    out.write(exposureTimeMode.hashBytes)

    out.close()
    bao.toByteArray

object AcquisitionConfig:

  given Eq[AcquisitionConfig] =
    Eq.by: a =>
      (
        a.exposureTimeMode,
        a.defaultFilter,
        a.explicitFilter
      )