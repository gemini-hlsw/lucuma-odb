// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.DaytimeCalibrationObservingModeType
import lucuma.core.enums.Instrument
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.InstrumentSetup

object DaytimeCalibrationInput:

  type Create = Config
  val  Create = Config

  case class Edit(
    instrument: Option[Instrument],
    sets:       Option[List[CalibrationSet[InstrumentSetup]]]
  ):
    def toCreate: Result[Create] =
      Result.fromOption(
        instrument.map(i => Create(i, sets.getOrElse(Nil))),
        "Cannot turn edit into create. `instrument` must be defined."
      )

  private val AllowedInstrumentBinding: Matcher[Instrument] =
    InstrumentBinding.rmap: i =>
      if DaytimeCalibrationObservingModeType.DaytimeCalibration.daytimeCalInstruments.contains(i) then Result.success(i)
      else Result.failure(s"Instrument ${i.tag} does not support daytime calibrations.")

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        AllowedInstrumentBinding("instrument", rInstrument),
        CalibrationSetInput.Binding.List.Option("calibrationSets", rSets)
      ) =>
        (rInstrument, rSets).parFlatMapN: (i, sets) =>
          val ss = sets.getOrElse(Nil)
          CalibrationSetInput.validateInstrument(i, ss).as(Create(i, ss))

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        AllowedInstrumentBinding.NonNullable("instrument", rInstrument),
        CalibrationSetInput.Binding.List.Option("calibrationSets", rSets)
      ) =>
        (rInstrument, rSets).parFlatMapN: (i, sets) =>
          (i, sets).tupled.fold(Result.unit)(CalibrationSetInput.validateInstrument).as(Edit(i, sets))
