// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.Instrument
import lucuma.odb.graphql.binding.GhostResolutionModeBinding
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.sequence.daytimecal.Config

object DaytimeCalibrationInput:

  case class Create(
    instrument:          Instrument,
    sets:                List[CalibrationSetInput],
    ghostResolutionMode: Option[GhostResolutionMode]
  ):
    def mode: DaytimeCalibrationObservingModeType =
      DaytimeCalibrationObservingModeType.DaytimeCalibration

  case class Edit(
    instrument:          Option[Instrument],
    sets:                Option[List[CalibrationSetInput]],
    ghostResolutionMode: Option[GhostResolutionMode]
  ):
    def toCreate: Result[Create] =
      Result
        .fromOption(instrument, "Cannot turn edit into create. `instrument` must be defined.")
        .flatMap: i =>
          validateGhost(i, ghostResolutionMode).as:
            Create(i, sets.getOrElse(Nil), Config.ghostResolutionModeFor(i, ghostResolutionMode))

  val GhostNotAllowed: String =
    "The `ghost` settings are only allowed when the instrument is GHOST."

  def validateGhost(instrument: Instrument, ghostResolutionMode: Option[GhostResolutionMode]): Result[Unit] =
    if ghostResolutionMode.isEmpty || instrument === Instrument.Ghost then Result.unit
    else Result.failure(GhostNotAllowed)

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
        GhostBinding.NonNullable("ghost", rGhost)
      ) =>
        (rInstrument, rSets, rGhost.map(_.flatten)).parFlatMapN: (i, sets, res) =>
          val ss = sets.getOrElse(Nil)
          (CalibrationSetInput.validateInstrument(i, ss.map(_.set)), validateGhost(i, res)).parTupled
            .as(Create(i, ss, Config.ghostResolutionModeFor(i, res)))

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        AllowedInstrumentBinding.NonNullable("instrument", rInstrument),
        CalibrationSetInput.Binding.List.Option("calibrationSets", rSets),
        GhostBinding.NonNullable("ghost", rGhost)
      ) =>
        (rInstrument, rSets, rGhost.map(_.flatten)).parFlatMapN: (i, sets, res) =>
          (
            (i, sets).tupled.fold(Result.unit)((inst, ss) => CalibrationSetInput.validateInstrument(inst, ss.map(_.set))),
            i.fold(Result.unit)(validateGhost(_, res))
          ).parTupled.as(Edit(i, sets, res))
