// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import cats.data.NonEmptySet
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.StepConfig.Gcal.Lamp
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.InstrumentSetup

object CalibrationSetInput:

  private case class GcalFields(
    continuum: Option[GcalContinuum],
    arcs:      List[GcalArc],
    filter:    GcalFilter,
    diffuser:  Option[GcalDiffuser],
    shutter:   GcalShutter
  )

  private val GcalBinding: Matcher[GcalFields] =
    ObjectFieldsBinding.rmap:
      case List(
        GcalContinuumBinding.Option("continuum", rContinuum),
        GcalArcBinding.List.Option("arcs", rArcs),
        GcalFilterBinding("filter", rFilter),
        GcalDiffuserBinding.Option("diffuser", rDiffuser),
        GcalShutterBinding("shutter", rShutter)
      ) => (rContinuum, rArcs, rFilter, rDiffuser, rShutter).parMapN: (c, a, f, d, s) =>
        GcalFields(c, a.toList.flatten, f, d, s)

  def defaultDiffuser(instrument: Instrument): GcalDiffuser =
    instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth | Instrument.Ghost => GcalDiffuser.Visible
      case _                                                              => GcalDiffuser.Ir

  private def gcalFor(kind: CalibrationSetType, fields: Option[GcalFields], instrument: Instrument): Result[Option[Gcal]] =
    (kind, fields) match
      case (CalibrationSetType.Flat, Some(g)) =>
        (g.continuum, g.arcs) match
          case (Some(c), Nil) => Gcal(Lamp.fromContinuum(c), g.filter, g.diffuser.getOrElse(defaultDiffuser(instrument)), g.shutter).some.success
          case _              => Result.failure("A FLAT calibration set needs a continuum lamp and no arc lamps.")
      case (CalibrationSetType.Arc, Some(g))  =>
        (g.continuum, g.arcs) match
          case (None, a :: as) => Gcal(Lamp.fromArcs(NonEmptySet.of(a, as*)), g.filter, g.diffuser.getOrElse(defaultDiffuser(instrument)), g.shutter).some.success
          case _               => Result.failure("An ARC calibration set needs at least one arc lamp and no continuum lamp.")
      case (CalibrationSetType.Flat | CalibrationSetType.Arc, None) =>
        Result.failure(s"A ${kind.tag.toUpperCase} calibration set needs a `gcal` configuration.")
      case (_, Some(_)) =>
        Result.failure(s"A ${kind.tag.toUpperCase} calibration set takes no `gcal` configuration.")
      case (_, None) =>
        none.success

  private def zeroExposure(kind: CalibrationSetType, setup: InstrumentSetup): InstrumentSetup =
    if kind =!= CalibrationSetType.Bias then setup
    else setup match
      case InstrumentSetup.GmosNorth(c) => InstrumentSetup.GmosNorth(c.copy(exposure = TimeSpan.Zero))
      case InstrumentSetup.GmosSouth(c) => InstrumentSetup.GmosSouth(c.copy(exposure = TimeSpan.Zero))

  val Binding: Matcher[CalibrationSet[InstrumentSetup]] =
    ObjectFieldsBinding.rmap:
      case List(
        CalibrationSetTypeBinding("kind", rKind),
        PosIntBinding("count", rCount),
        GcalBinding.Option("gcal", rGcal),
        GmosNorthDynamicInput.Binding.Option("gmosNorth", rNorth),
        GmosSouthDynamicInput.Binding.Option("gmosSouth", rSouth)
      ) =>
        val rSetup: Result[InstrumentSetup] =
          (rNorth, rSouth).parFlatMapN:
            case (Some(n), None) => InstrumentSetup.GmosNorth(n).success
            case (None, Some(s)) => InstrumentSetup.GmosSouth(s).success
            case _               => Result.failure("Exactly one instrument setup (`gmosNorth` or `gmosSouth`) must be given.")

        (rKind, rCount, rGcal, rSetup).parFlatMapN: (kind, count, gcal, setup) =>
          gcalFor(kind, gcal, setup.instrument).map: g =>
            CalibrationSet(kind, count, g, zeroExposure(kind, setup))

  /** The complaint, if any, about a set whose setup is not for the mode's instrument. */
  def instrumentMismatch(instrument: Instrument, sets: List[CalibrationSet[InstrumentSetup]]): Option[String] =
    sets.zipWithIndex.collectFirst:
      case (s, i) if s.setup.instrument =!= instrument =>
        s"Calibration set $i is for ${s.setup.instrument.longName} but the daytime calibration is for ${instrument.longName}."

  def validateInstrument(instrument: Instrument, sets: List[CalibrationSet[InstrumentSetup]]): Result[Unit] =
    instrumentMismatch(instrument, sets).fold(Result.unit)(Result.failure)
