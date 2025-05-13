// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class RecordStepInput[A](
  atomId:          Atom.Id,
  instrument:      A,
  stepConfig:      StepConfig,
  telescopeConfig: TelescopeConfig,
  observeClass:    ObserveClass,
  generatedId:     Option[Step.Id]
)

object RecordStepInput:

  private def binding[A](
    instrumentName:    String,
    instrumentMatcher: Matcher[A]
  ): Matcher[RecordStepInput[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        AtomIdBinding("atomId", rAtomId),
        instrumentMatcher(`instrumentName`, rInstrument),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        TelescopeConfigInput.Binding.Option("telescopeConfig", rTelescopeConfig),
        ObserveClassBinding("observeClass", rObserveClass),
        StepIdBinding.Option("generatedId", rGenerated)
      ) => (rAtomId, rInstrument, rStepConfig, rTelescopeConfig, rObserveClass, rGenerated).parMapN {
        (atomId, instrument, step, telescope, oclass, generated) =>
        RecordStepInput(atomId, instrument, step, telescope.getOrElse(TelescopeConfig.Default), oclass, generated)
      }
    }

  val Flamingos2Binding: Matcher[RecordStepInput[Flamingos2DynamicConfig]] =
    binding("flamingos2", Flamingos2DynamicInput.Binding)

  val GmosNorthBinding: Matcher[RecordStepInput[GmosNorth]] =
    binding("gmosNorth", GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[RecordStepInput[GmosSouth]] =
    binding("gmosSouth", GmosSouthDynamicInput.Binding)