// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Instrument
import lucuma.odb.graphql.binding.GhostResolutionModeBinding
import lucuma.odb.graphql.binding.GnirsWellDepthBinding
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.sequence.daytimecal.Config

object DaytimeCalibrationInput:

  case class Create(
    instrument:          Instrument,
    sets:                List[CalibrationSetInput],
    ghostResolutionMode: Option[GhostResolutionMode],
    gnirsWellDepth:      Option[GnirsWellDepth]
  ):
    def mode: DaytimeCalibrationObservingModeType =
      DaytimeCalibrationObservingModeType.DaytimeCalibration

  case class Edit(
    instrument:          Option[Instrument],
    sets:                Option[List[CalibrationSetInput]],
    ghostResolutionMode: Option[GhostResolutionMode],
    gnirsWellDepth:      Option[GnirsWellDepth]
  ):
    def toCreate: Result[Create] =
      Result
        .fromOption(instrument, "Cannot turn edit into create. `instrument` must be defined.")
        .flatMap: i =>
          (validateGhost(i, ghostResolutionMode), validateGnirs(i, gnirsWellDepth)).parTupled.as:
            Create(i, sets.getOrElse(Nil), Config.ghostResolutionModeFor(i, ghostResolutionMode), Config.gnirsWellDepthFor(i, gnirsWellDepth))

  val GhostNotAllowed: String =
    "The `ghost` settings are only allowed when the instrument is GHOST."

  def validateGhost(instrument: Instrument, ghostResolutionMode: Option[GhostResolutionMode]): Result[Unit] =
    if ghostResolutionMode.isEmpty || instrument === Instrument.Ghost then Result.unit
    else Result.failure(GhostNotAllowed)

  val GnirsNotAllowed: String =
    "The `gnirs` settings are only allowed when the instrument is GNIRS."

  def validateGnirs(instrument: Instrument, gnirsWellDepth: Option[GnirsWellDepth]): Result[Unit] =
    if gnirsWellDepth.isEmpty || instrument === Instrument.Gnirs then Result.unit
    else Result.failure(GnirsNotAllowed)

  private val GnirsBinding: Matcher[Option[GnirsWellDepth]] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsWellDepthBinding.NonNullable("wellDepth", rWellDepth)
      ) => rWellDepth

  private val GhostBinding: Matcher[Option[GhostResolutionMode]] =
    ObjectFieldsBinding.rmap:
      case List(
        GhostResolutionModeBinding.NonNullable("resolutionMode", rResolutionMode)
      ) => rResolutionMode

  private val AllowedInstrumentBinding: Matcher[Instrument] =
    InstrumentBinding.rmap: i =>
      if DaytimeCalibrationObservingModeType.DaytimeCalibration.daytimeCalInstruments.contains(i) then Result.success(i)
      else Result.failure(s"Instrument ${i.tag} does not support daytime calibrations.")

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        AllowedInstrumentBinding("instrument", rInstrument),
        CalibrationSetInput.Binding.List.Option("calibrationSets", rSets),
        GhostBinding.NonNullable("ghost", rGhost),
        GnirsBinding.NonNullable("gnirs", rGnirs)
      ) =>
        (rInstrument, rSets, rGhost.map(_.flatten), rGnirs.map(_.flatten)).parFlatMapN: (i, sets, res, wd) =>
          val ss = sets.getOrElse(Nil)
          (CalibrationSetInput.validateInstrument(i, ss.map(_.set)), validateGhost(i, res), validateGnirs(i, wd)).parTupled
            .as(Create(i, ss, Config.ghostResolutionModeFor(i, res), Config.gnirsWellDepthFor(i, wd)))

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        AllowedInstrumentBinding.NonNullable("instrument", rInstrument),
        CalibrationSetInput.Binding.List.Option("calibrationSets", rSets),
        GhostBinding.NonNullable("ghost", rGhost),
        GnirsBinding.NonNullable("gnirs", rGnirs)
      ) =>
        (rInstrument, rSets, rGhost.map(_.flatten), rGnirs.map(_.flatten)).parFlatMapN: (i, sets, res, wd) =>
          (
            (i, sets).tupled.fold(Result.unit)((inst, ss) => CalibrationSetInput.validateInstrument(inst, ss.map(_.set))),
            i.fold(Result.unit)(validateGhost(_, res)),
            i.fold(Result.unit)(validateGnirs(_, wd))
          ).parTupled.as(Edit(i, sets, res, wd))
