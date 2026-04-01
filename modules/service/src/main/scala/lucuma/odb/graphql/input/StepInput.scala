// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.ProtoStep

object StepInput:

  private def binding[D](
    instrumentMatcher: Matcher[D]
  ): Matcher[ProtoStep[D]] =
    ObjectFieldsBinding.rmap:
      case List(
        instrumentMatcher("instrumentConfig", rInstrument),
        BreakpointBinding.Option("breakpoint", rBreakpoint),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        TelescopeConfigInput.Binding.Option("telescopeConfig", rTelescopeConfig),
        ObserveClassBinding("observeClass", rObserveClass),
      ) => (rInstrument, rBreakpoint, rStepConfig, rTelescopeConfig, rObserveClass).parMapN:
        (instrument, breakpoint, step, telescope, oclass) =>
          ProtoStep(
            instrument,
            step,
            telescope.getOrElse(TelescopeConfig.Default),
            oclass,
            breakpoint.getOrElse(Breakpoint.Disabled)
          )

  val Flamingos2Binding: Matcher[ProtoStep[Flamingos2DynamicConfig]] =
    binding(Flamingos2DynamicInput.Binding)

  val GmosNorthBinding: Matcher[ProtoStep[GmosNorth]] =
    binding(GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[ProtoStep[GmosSouth]] =
    binding(GmosSouthDynamicInput.Binding)

  val Igrins2Binding: Matcher[ProtoStep[Igrins2DynamicConfig]] =
    binding(Igrins2DynamicInput.Binding)
