// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.derived.*
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.sequence.util.HashBytes.given

import scala.collection.mutable.ArrayBuilder

/**
 * Parameters required for producing ITC inputs.  These are extracted from an
 * observation when everything necessary is present and defined.
 */
case class ItcInput(
  imaging:             ImagingParameters,
  spectroscopy:        SpectroscopyParameters,
  targets:             NonEmptyList[(Target.Id, TargetInput, Option[Timestamp])],
) derives Eq:
  lazy val imagingInput: ImagingInput =
    ImagingInput(imaging, targets.map(_._2))

  lazy val spectroscopyInput: SpectroscopyInput =
    SpectroscopyInput(spectroscopy, targets.map(_._2))

  lazy val targetVector: NonEmptyVector[(Target.Id, TargetInput)] =
    targets.map(t => (t._1, t._2)).toNev

object ItcInput:

  given HashBytes[ItcInput] with
    given HashBytes[TargetInput]            = HashBytes.forJsonEncoder
    given HashBytes[ImagingParameters]      = HashBytes.forJsonEncoder
    given HashBytes[SpectroscopyParameters] = HashBytes.forJsonEncoder

    def hashBytes(a: ItcInput): Array[Byte] =
      def targetsBytes(
        targets: NonEmptyList[(Target.Id, TargetInput, Option[Timestamp])]
      ): Array[Byte] =
        val bld = ArrayBuilder.make[Byte]
        targets.toList.sortBy(_._1).foreach: (tid, tinput, customSedTimestamp) =>
          bld.addAll(tid.hashBytes)
          bld.addAll(tinput.hashBytes)
          bld.addAll(customSedTimestamp.hashBytes)
        bld.result()

      Array.concat(
        a.imaging.hashBytes,
        a.spectroscopy.hashBytes,
        targetsBytes(a.targets),
      )
