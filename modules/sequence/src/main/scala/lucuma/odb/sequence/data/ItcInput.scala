// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.Order
import cats.Order.given
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
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
import monocle.Prism

import scala.collection.mutable.ArrayBuilder

/**
 * A simple ITC input creation ADT, separating imaging vs spectroscopy.
 */
sealed trait ItcInput:
  def targets: NonEmptyList[ItcInput.TargetDefinition]

object ItcInput:

  private given HashBytes[TargetInput]            = HashBytes.forJsonEncoder
  private given HashBytes[ImagingParameters]      = HashBytes.forJsonEncoder
  private given HashBytes[SpectroscopyParameters] = HashBytes.forJsonEncoder

  case class TargetDefinition(
    targetId: Target.Id,
    input:    TargetInput,
    time:     Option[Timestamp]
  ) derives Eq

  private def hashTargets(nel: NonEmptyList[TargetDefinition]): Array[Byte] =
    val bld = ArrayBuilder.make[Byte]
    nel.toList.sortBy(_.targetId).foreach: t =>
      bld.addAll(t.targetId.hashBytes)
      bld.addAll(t.input.hashBytes)
      bld.addAll(t.time.hashBytes)
    bld.result()

  /**
   * ImagingInputs per-filter (as contained in the IntrumentMode in
   * ImagingParameters).
   */
  case class Imaging(
    science: NonEmptyList[ImagingParameters],
    targets: NonEmptyList[TargetDefinition],
  ) extends ItcInput derives Eq:

    def scienceInput: NonEmptyList[ImagingInput] =
      science.map(ImagingInput(_, targets.map(_.input)))

  object Imaging:
    given HashBytes[Imaging] with
      def hashBytes(a: Imaging): Array[Byte] =
        val bld = ArrayBuilder.make[Byte]
        a.science.toList.foreach: params =>
          bld.addAll(params.hashBytes)
        bld.addAll(hashTargets(a.targets))
        bld.result()

  /**
   * Spectroscopy inputs include imaging parameters (for acquisition),
   * the main spectrocopy input, and an optional blind offset target.
   */
  case class Spectroscopy(
    acquisition: ImagingParameters,
    science:     SpectroscopyParameters,
    targets:     NonEmptyList[TargetDefinition],
    blindOffset: Option[TargetDefinition]
  ) extends ItcInput derives Eq:

    def acquisitionTargets: NonEmptyList[TargetDefinition] =
      blindOffset.fold(targets)(NonEmptyList.one)

    def acquisitionInput: ImagingInput =
      ImagingInput(acquisition, acquisitionTargets.map(_.input))

    def scienceInput: SpectroscopyInput =
      SpectroscopyInput(science, targets.map(_.input))

  object Spectroscopy:
    given HashBytes[Spectroscopy] with
      def hashBytes(a: Spectroscopy): Array[Byte] =
        Array.concat(
          a.acquisition.hashBytes,
          a.science.hashBytes,
          hashTargets(a.blindOffset.fold(a.targets)(_ :: a.targets))
        )

  /**
    * ItcInput for spectroscopy, for instruments where GPP does not manage
    * acquisition (IGRINS2, GHOST).
    */
  case class ScienceOnlySpectroscopy(
    science: SpectroscopyParameters,
    targets: NonEmptyList[TargetDefinition]
  ) extends ItcInput derives Eq:

    def scienceInput: SpectroscopyInput =
      SpectroscopyInput(science, targets.map(_.input))

  object ScienceOnlySpectroscopy:
    given HashBytes[ScienceOnlySpectroscopy] with
      def hashBytes(a: ScienceOnlySpectroscopy): Array[Byte] =
        Array.concat(
          a.science.hashBytes,
          hashTargets(a.targets)
        )

  val spectroscopy: Prism[ItcInput, ItcInput.Spectroscopy] =
    Prism[ItcInput, ItcInput.Spectroscopy] {
      case s: ItcInput.Spectroscopy => s.some
      case _                        => none
    }(identity)

  val scienceOnlySpectroscopy: Prism[ItcInput, ItcInput.ScienceOnlySpectroscopy] =
    Prism[ItcInput, ItcInput.ScienceOnlySpectroscopy] {
      case s: ItcInput.ScienceOnlySpectroscopy => s.some
      case _                                   => none
    }(identity)

  given Eq[ItcInput] =
    Eq.instance:
      case (n0: Imaging,                 n1: Imaging)                 => n0 === n1
      case (n0: Spectroscopy,            n1: Spectroscopy)            => n0 === n1
      case (n0: ScienceOnlySpectroscopy, n1: ScienceOnlySpectroscopy) => n0 === n1
      case _                                                          => false

  given HashBytes[ItcInput] with
    def hashBytes(a: ItcInput): Array[Byte] =
      a match
        case in @ Imaging(_, _)                  => in.hashBytes
        case in @ Spectroscopy(_, _, _, _)       => in.hashBytes
        case in @ ScienceOnlySpectroscopy(_, _)  => in.hashBytes