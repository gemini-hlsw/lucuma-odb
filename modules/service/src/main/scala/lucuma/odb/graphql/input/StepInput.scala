// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class StepInput[D](
  instrument:      D,
  breakpoint:      Breakpoint,
  stepConfig:      StepConfig,
  telescopeConfig: TelescopeConfig,
  observeClass:    ObserveClass
)

object StepInput:

  private def binding[D](
    instrumentMatcher: Matcher[D]
  ): Matcher[StepInput[D]] =
    ObjectFieldsBinding.rmap:
      case List(
        instrumentMatcher("instrumentConfig", rInstrument),
        BreakpointBinding.Option("breakpoint", rBreakpoint),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        TelescopeConfigInput.Binding.Option("telescopeConfig", rTelescopeConfig),
        ObserveClassBinding("observeClass", rObserveClass),
      ) => (rInstrument, rBreakpoint, rStepConfig, rTelescopeConfig, rObserveClass).parMapN:
        (instrument, breakpoint, step, telescope, oclass) =>
          StepInput(instrument, breakpoint.getOrElse(Breakpoint.Disabled), step, telescope.getOrElse(TelescopeConfig.Default), oclass)

  val Flamingos2Binding: Matcher[StepInput[Flamingos2DynamicConfig]] =
    binding(Flamingos2DynamicInput.Binding)

  val GmosNorthBinding: Matcher[StepInput[GmosNorth]] =
    binding(GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[StepInput[GmosSouth]] =
    binding(GmosSouthDynamicInput.Binding)