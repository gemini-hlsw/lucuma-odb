// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import lucuma.core.model.Target
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.ImagingIntegrationTimeParameters
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import scala.collection.mutable.ArrayBuilder

case class ItcInput(
  imaging:      ImagingIntegrationTimeParameters,
  spectroscopy: SpectroscopyIntegrationTimeParameters,
  targets:      NonEmptyList[(Target.Id, TargetInput)]
):
  lazy val imagingInput: ImagingIntegrationTimeInput =
    ImagingIntegrationTimeInput(imaging, targets.map(_._2))

  lazy val spectroscopyInput: SpectroscopyIntegrationTimeInput =
    SpectroscopyIntegrationTimeInput(spectroscopy, targets.map(_._2))

  lazy val targetVector: NonEmptyVector[(Target.Id, TargetInput)] =
    targets.toNev

object ItcInput:

  given Eq[ItcInput] =
    Eq.by { a => (a.imaging, a.spectroscopy, a.targets) }

  given HashBytes[ItcInput] with
    given HashBytes[TargetInput]                           = HashBytes.forJsonEncoder
    given HashBytes[ImagingIntegrationTimeParameters]      = HashBytes.forJsonEncoder
    given HashBytes[SpectroscopyIntegrationTimeParameters] = HashBytes.forJsonEncoder

    def hashBytes(a: ItcInput): Array[Byte] =
      def targetsBytes(
        targets: NonEmptyList[(Target.Id, TargetInput)]
      ): Array[Byte] =
        val bld = ArrayBuilder.make[Byte]
        targets.toList.sortBy(_._1).foreach: (tid, tinput) =>
          bld.addAll(tid.hashBytes)
          bld.addAll(tinput.hashBytes)
        bld.result()

      Array.concat(
        a.imaging.hashBytes,
        a.spectroscopy.hashBytes,
        targetsBytes(a.targets)
      )