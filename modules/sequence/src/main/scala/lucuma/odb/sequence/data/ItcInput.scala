// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import io.circe.Encoder
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.itc.AltairParameters
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.sequence.util.HashBytes.given
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

import scala.collection.mutable.ArrayBuilder

/**
 * A simple ITC input creation ADT, separating imaging vs spectroscopy.
 */
sealed trait ItcInput:
  def targets: NonEmptyList[ItcInput.TargetDefinition]

  /**
   * The asterism target the user selected to drive the signal-to-noise
   * calculation, if any. When present, the ITC "selected" result is pinned to
   * this target rather than chosen automatically (brightest).
   */
  def signalToNoiseTargetId: Option[Target.Id]

  /**
   * The Altair parameters the GNIRS modes of this input carry, if any. They come from the guide
   * star the sequence generator resolves, not from the database, and are keyed apart from the
   * input hash for that reason; see [[ItcInput.withAltairParameters]].
   */
  def altairParameters: Option[AltairParameters] =
    this match
      case i @ ItcInput.Imaging(science = _)           => ItcInput.modeAltair(i.science.head.mode)
      case i @ ItcInput.GnirsSpectroscopy(science = _) => ItcInput.modeAltair(i.science.head.mode)
      case _                                           => none

object ItcInput:

  private def modeAltair(mode: InstrumentMode): Option[AltairParameters] =
    mode match
      case m @ InstrumentMode.GnirsImaging(filter = _)                 => m.altair
      case m @ InstrumentMode.GnirsSpectroscopy(centralWavelength = _) => m.altair
      case _                                                           => none

  private def setModeAltair(altair: Option[AltairParameters])(mode: InstrumentMode): InstrumentMode =
    mode match
      case m @ InstrumentMode.GnirsImaging(filter = _)                 => m.copy(altair = altair)
      case m @ InstrumentMode.GnirsSpectroscopy(centralWavelength = _) => m.copy(altair = altair)
      case m                                                           => m

  /**
   * Rewrites the Altair parameters of every GNIRS mode of an input. Only GNIRS observes behind
   * Altair, so every other mode is left alone.
   */
  def withAltairParameters(input: ItcInput, altair: Option[AltairParameters]): ItcInput =
    val set: InstrumentMode => InstrumentMode = setModeAltair(altair)
    input match
      case i @ Imaging(science = _)           =>
        i.copy(
          science     = i.science.map(ImagingParameters.mode.modify(set)),
          acquisition = i.acquisition.map(ImagingParameters.mode.modify(set))
        )
      case i @ GnirsSpectroscopy(science = _) =>
        i.copy(
          acquisition = ImagingParameters.mode.modify(set)(i.acquisition),
          science     = i.science.map(SpectroscopyParameters.mode.modify(set))
        )
      case i                                  =>
        i

  /**
   * Hashes the ITC parameters with the Altair parameters removed. Altair is modelled from a guide
   * star that only sequence generation resolves, so hashing it here would give the generator and a
   * reader that has the database alone different keys for the same observation. The ITC result
   * cache keys the Altair parameters separately instead (t_itc_result.c_altair_hash).
   */
  private def hashBytesWithoutAltair[A: Encoder](mode: Lens[A, InstrumentMode]): HashBytes[A] =
    val json: HashBytes[A] = HashBytes.forJsonEncoder
    a => json.hashBytes(mode.modify(setModeAltair(none))(a))

  private given HashBytes[TargetInput]            = HashBytes.forJsonEncoder
  private given HashBytes[ImagingParameters]      = hashBytesWithoutAltair(ImagingParameters.mode)
  private given HashBytes[SpectroscopyParameters] = hashBytesWithoutAltair(SpectroscopyParameters.mode)

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

   * GNIRS imaging has an acquisition sequence sized by its own ITC pass; other
   * imaging modes have none.  When `gnirsAcqAutoClassify` is set, the ITC resolves
   * the acquisition brightness type via a classification pass before the real
   * exposure-time pass.  See the two-pass acquisition ITC in ItcService.
   *
   * `gnirsAcqAutoSignalToNoise` is set when the acquisition signal-to-noise is
   * itself derived from that classification, in which case the second pass runs at
   * the derived S/N rather than at the one carried here.
   */
  case class Imaging(
    science: NonEmptyList[ImagingParameters],
    targets: NonEmptyList[TargetDefinition],
    signalToNoiseTargetId: Option[Target.Id],
    acquisition:          Option[ImagingParameters] = None,
    gnirsAcqAutoClassify: Boolean                   = false,
    gnirsAcqAutoSignalToNoise: Boolean              = false
  ) extends ItcInput derives Eq:

    def scienceInput: NonEmptyList[ImagingInput] =
      science.map(ImagingInput(_, targets.map(_.input)))

    // Imaging has no blind offset, so the acquisition uses the science targets.
    def acquisitionTargets: NonEmptyList[TargetDefinition] =
      targets

    def acquisitionInput: Option[ImagingInput] =
      acquisition.map(ImagingInput(_, acquisitionTargets.map(_.input)))

  object Imaging:
    given HashBytes[Imaging] with
      def hashBytes(a: Imaging): Array[Byte] =
        val bld = ArrayBuilder.make[Byte]
        a.science.toList.foreach: params =>
          bld.addAll(params.hashBytes)
        bld.addAll(hashTargets(a.targets))
        bld.addAll(a.signalToNoiseTargetId.hashBytes)
        bld.addAll(a.acquisition.hashBytes)
        bld.addAll(a.gnirsAcqAutoClassify.hashBytes)
        bld.addAll(a.gnirsAcqAutoSignalToNoise.hashBytes)
        bld.result()

  /**
   * Spectroscopy inputs include imaging parameters (for acquisition),
   * the main spectrocopy input, and an optional blind offset target.
   * 
   * When `gnirsAcqAutoClassify` is set (in the case we are in GNIRS S/N mode
   * with acquisition mode and filter both auto), the ITC resolves the
   * acquisition mode via a brightness classification pass before the real
   * exposure-time pass. See the two-pass acquisition ITC in ItcService.
   *
   * `gnirsAcqAutoSignalToNoise` is set when the acquisition signal-to-noise is
   * itself derived from that classification, in which case the second pass runs at
   * the derived S/N rather than at the one carried here.
   */
  case class Spectroscopy(
    acquisition: ImagingParameters,
    science:     SpectroscopyParameters,
    targets:     NonEmptyList[TargetDefinition],
    blindOffset: Option[TargetDefinition],
    signalToNoiseTargetId: Option[Target.Id],
    gnirsAcqAutoClassify: Boolean = false,
    gnirsAcqAutoSignalToNoise: Boolean = false
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
          hashTargets(a.blindOffset.fold(a.targets)(_ :: a.targets)),
          a.signalToNoiseTargetId.hashBytes,
          a.gnirsAcqAutoClassify.hashBytes,
          a.gnirsAcqAutoSignalToNoise.hashBytes
        )

  /**
   * GNIRS spectroscopy takes spectra at one or more central wavelengths, each a
   * separate configuration with its own exposure time mode and coadds, and so
   * its own ITC calculation.  The acquisition is still a single pass.
   *
   * This is the spectroscopy analogue of [[Imaging]]'s per-filter fan out; the
   * other spectroscopy modes have exactly one science configuration and use
   * [[Spectroscopy]].
   */
  case class GnirsSpectroscopy(
    acquisition:           ImagingParameters,
    science:               NonEmptyList[SpectroscopyParameters],
    targets:               NonEmptyList[TargetDefinition],
    blindOffset:           Option[TargetDefinition],
    signalToNoiseTargetId: Option[Target.Id],
    gnirsAcqAutoClassify:  Boolean = false,
    gnirsAcqAutoSignalToNoise: Boolean = false
  ) extends ItcInput derives Eq:

    def acquisitionTargets: NonEmptyList[TargetDefinition] =
      blindOffset.fold(targets)(NonEmptyList.one)

    def acquisitionInput: ImagingInput =
      ImagingInput(acquisition, acquisitionTargets.map(_.input))

    def scienceInput: NonEmptyList[SpectroscopyInput] =
      science.map(SpectroscopyInput(_, targets.map(_.input)))

  object GnirsSpectroscopy:
    given HashBytes[GnirsSpectroscopy] with
      def hashBytes(a: GnirsSpectroscopy): Array[Byte] =
        val bld = ArrayBuilder.make[Byte]
        bld.addAll(a.acquisition.hashBytes)
        // Every science configuration must contribute: two observations that
        // differ only in an extra central wavelength must not share a cache key.
        a.science.toList.foreach: params =>
          bld.addAll(params.hashBytes)
        bld.addAll(hashTargets(a.blindOffset.fold(a.targets)(_ :: a.targets)))
        bld.addAll(a.signalToNoiseTargetId.hashBytes)
        bld.addAll(a.gnirsAcqAutoClassify.hashBytes)
        bld.addAll(a.gnirsAcqAutoSignalToNoise.hashBytes)
        bld.result()

  val gnirsSpectroscopy: Prism[ItcInput, ItcInput.GnirsSpectroscopy] =
    GenPrism[ItcInput, ItcInput.GnirsSpectroscopy]

  /**
    * ItcInput for spectroscopy, for instruments where GPP does not manage
    * acquisition (IGRINS2, GHOST).
    */
  case class ScienceOnlySpectroscopy(
    science: SpectroscopyParameters,
    targets: NonEmptyList[TargetDefinition],
    signalToNoiseTargetId: Option[Target.Id]
  ) extends ItcInput derives Eq:

    def scienceInput: SpectroscopyInput =
      SpectroscopyInput(science, targets.map(_.input))

  object ScienceOnlySpectroscopy:
    given HashBytes[ScienceOnlySpectroscopy] with
      def hashBytes(a: ScienceOnlySpectroscopy): Array[Byte] =
        Array.concat(
          a.science.hashBytes,
          hashTargets(a.targets),
          a.signalToNoiseTargetId.hashBytes
        )

  val spectroscopy: Prism[ItcInput, ItcInput.Spectroscopy] =
    GenPrism[ItcInput, ItcInput.Spectroscopy]

  val scienceOnlySpectroscopy: Prism[ItcInput, ItcInput.ScienceOnlySpectroscopy] =
    GenPrism[ItcInput, ItcInput.ScienceOnlySpectroscopy]

  given Eq[ItcInput] =
    Eq.instance:
      case (n0: Imaging,                 n1: Imaging)                 => n0 === n1
      case (n0: Spectroscopy,            n1: Spectroscopy)            => n0 === n1
      case (n0: GnirsSpectroscopy,       n1: GnirsSpectroscopy)       => n0 === n1
      case (n0: ScienceOnlySpectroscopy, n1: ScienceOnlySpectroscopy) => n0 === n1
      case _                                                          => false

  given HashBytes[ItcInput] with
    def hashBytes(a: ItcInput): Array[Byte] =
      a match
        case in @ Imaging(science = _)                 => in.hashBytes
        case in @ Spectroscopy(science = _)            => in.hashBytes
        case in @ GnirsSpectroscopy(science = _)       => in.hashBytes
        case in @ ScienceOnlySpectroscopy(science = _) => in.hashBytes
