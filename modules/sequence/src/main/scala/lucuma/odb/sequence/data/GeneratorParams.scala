// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import java.nio.charset.StandardCharsets.UTF_8

case class GeneratorParams(
  itc:             NonEmptyList[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))],
  observingMode:   ObservingMode,
  calibrationRole: Option[CalibrationRole]
)

object GeneratorParams {

  given Eq[GeneratorParams] =
    Eq.by { a => (
      a.itc,
      a.observingMode,
      a.calibrationRole
    )}

  private def itcBytes(
    itc: NonEmptyList[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))]
  ): Array[Byte] = {
    val bld = scala.collection.mutable.ArrayBuilder.make[Byte]

    given HashBytes[ImagingIntegrationTimeInput] = HashBytes.forJsonEncoder
    given HashBytes[SpectroscopyIntegrationTimeInput] = HashBytes.forJsonEncoder

    itc.toList.foreach { case (tid, (imaging, spectroscopy)) =>
      bld.addAll(tid.hashBytes)
      bld.addAll(imaging.hashBytes)
      bld.addAll(spectroscopy.hashBytes)
    }

    bld.result()
  }

  given HashBytes[GeneratorParams] with
    def hashBytes(a: GeneratorParams): Array[Byte] =
      Array.concat(
        itcBytes(a.itc),
        a.observingMode.hashBytes,
        a.calibrationRole.fold(Array.emptyByteArray)(_.tag.getBytes(UTF_8))
      )

}
