// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.ImagingIntegrationTimeParameters
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import java.nio.charset.StandardCharsets.UTF_8

case class GeneratorAsterismParams(
  imaging: ImagingIntegrationTimeParameters,
  spectroscopy: SpectroscopyIntegrationTimeParameters,
  targets: NonEmptyList[(Target.Id, TargetInput)]
) {
  lazy val imagingInput: ImagingIntegrationTimeInput = 
    ImagingIntegrationTimeInput(imaging, targets.map(_._2))

  lazy val spectroscopyInput: SpectroscopyIntegrationTimeInput = 
    SpectroscopyIntegrationTimeInput(spectroscopy, targets.map(_._2))

  lazy val targetVector: NonEmptyVector[(Target.Id, TargetInput)] = targets.toNev
}

object GeneratorAsterismParams {
  given Eq[GeneratorAsterismParams] =
    Eq.by { a => (
      a.imaging,
      a.spectroscopy,
      a.targets
    )}

  given HashBytes[ImagingIntegrationTimeParameters] = HashBytes.forJsonEncoder
  given HashBytes[SpectroscopyIntegrationTimeParameters] = HashBytes.forJsonEncoder

  private def targetsBytes(targets: NonEmptyList[(Target.Id, TargetInput)]): Array[Byte] = {
      val bld = scala.collection.mutable.ArrayBuilder.make[Byte]

      given HashBytes[TargetInput] = HashBytes.forJsonEncoder

      targets.toList.sortBy(_._1).foreach { case (tid, tinput) =>
        bld.addAll(tid.hashBytes)
        bld.addAll(tinput.hashBytes)
      }

      bld.result()
    }

  given HashBytes[GeneratorAsterismParams] with
    def hashBytes(a: GeneratorAsterismParams): Array[Byte] =
      Array.concat(
        a.imaging.hashBytes,
        a.spectroscopy.hashBytes,
        targetsBytes(a.targets)
      )
}

case class GeneratorParams(
  itc:             GeneratorAsterismParams,
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


  given HashBytes[GeneratorParams] with
    def hashBytes(a: GeneratorParams): Array[Byte] =
      Array.concat(
        a.itc.hashBytes,
        a.observingMode.hashBytes,
        a.calibrationRole.fold(Array.emptyByteArray)(_.tag.getBytes(UTF_8))
      )

}
