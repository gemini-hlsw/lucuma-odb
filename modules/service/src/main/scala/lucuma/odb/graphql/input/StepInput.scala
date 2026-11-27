// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.OffsetPurpose
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfigSpec
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.ProtoStep

object StepInput:

  /**
   * A step as given in a sequence edit.  Its telescope position may leave the
   * purpose unspecified, to be derived from the observation's observing mode
   * when the step is stored.
   */
  final case class Spec[D](
    value:           D,
    stepConfig:      StepConfig,
    telescopeConfig: TelescopeConfigSpec,
    observeClass:    ObserveClass,
    breakpoint:      Breakpoint
  ):
    def resolve(defaultPurpose: Offset => OffsetPurpose): ProtoStep[D] =
      ProtoStep(value, stepConfig, telescopeConfig.toTelescopeConfig(defaultPurpose), observeClass, breakpoint)

  private def binding[D](
    instrumentMatcher: Matcher[D]
  ): Matcher[Spec[D]] =
    ObjectFieldsBinding.rmap:
      case List(
        instrumentMatcher("instrumentConfig", rInstrument),
        BreakpointBinding.Option("breakpoint", rBreakpoint),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        TelescopeConfigInput.Binding.Option("telescopeConfig", rTelescopeConfig),
        ObserveClassBinding("observeClass", rObserveClass),
      ) => (rInstrument, rBreakpoint, rStepConfig, rTelescopeConfig, rObserveClass).parMapN:
        (instrument, breakpoint, step, telescope, oclass) =>
          Spec(
            instrument,
            step,
            telescope.getOrElse(TelescopeConfigSpec.Default),
            oclass,
            breakpoint.getOrElse(Breakpoint.Disabled)
          )

  val Flamingos2Binding: Matcher[Spec[Flamingos2DynamicConfig]] =
    binding(Flamingos2DynamicInput.Binding)

  val GmosNorthBinding: Matcher[Spec[GmosNorth]] =
    binding(GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[Spec[GmosSouth]] =
    binding(GmosSouthDynamicInput.Binding)

  val Igrins2Binding: Matcher[Spec[Igrins2DynamicConfig]] =
    binding(Igrins2DynamicInput.Binding)

  val GnirsBinding: Matcher[Spec[GnirsDynamicConfig]] =
    binding(GnirsDynamicInput.Binding)

  val GhostBinding: Matcher[Spec[GhostDynamicConfig]] =
    binding(GhostDynamicInput.Binding)
