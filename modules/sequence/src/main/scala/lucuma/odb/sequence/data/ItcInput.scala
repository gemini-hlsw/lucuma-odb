// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import lucuma.core.model.Target
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import scala.collection.mutable.ArrayBuilder

/**
 * Parameters required for producing ITC inputs.  These are extracted from an
 * observation when everything necessary is present and defined.
 */
case class ItcInput(
  imaging:      ImagingParameters,
  spectroscopy: SpectroscopyParameters,
  targets:      NonEmptyList[(Target.Id, TargetInput)]
):
  lazy val imagingInput: ImagingInput =
    ImagingInput(imaging, targets.map(_._2))

  lazy val spectroscopyInput: SpectroscopyInput =
    SpectroscopyInput(spectroscopy, targets.map(_._2))

  lazy val targetVector: NonEmptyVector[(Target.Id, TargetInput)] =
    targets.toNev

object ItcInput:

  given Eq[ItcInput] =
    Eq.by { a => (a.imaging, a.spectroscopy, a.targets) }

  given HashBytes[ItcInput] with
    given HashBytes[TargetInput]            = HashBytes.forJsonEncoder
    given HashBytes[ImagingParameters]      = HashBytes.forJsonEncoder
    given HashBytes[SpectroscopyParameters] = HashBytes.forJsonEncoder

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
