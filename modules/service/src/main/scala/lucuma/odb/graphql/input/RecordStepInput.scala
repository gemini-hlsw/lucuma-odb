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
import lucuma.core.util.IdempotencyKey
import lucuma.odb.graphql.binding.*

case class RecordStepInput[A](
  atomId:          Atom.Id,
  instrument:      A,
  stepConfig:      StepConfig,
  telescopeConfig: TelescopeConfig,
  observeClass:    ObserveClass,
  generatedId:     Option[Step.Id],
  idempotencyKey:  Option[IdempotencyKey]
)

object RecordStepInput:

  private def binding[A](
    instrumentName:    String,
    instrumentMatcher: Matcher[A]
  ): Matcher[RecordStepInput[A]] =
    ObjectFieldsBinding.rmap:
      case List(
        AtomIdBinding("atomId", rAtomId),
        instrumentMatcher(`instrumentName`, rInstrument),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        TelescopeConfigInput.Binding.Option("telescopeConfig", rTelescopeConfig),
        ObserveClassBinding("observeClass", rObserveClass),
        StepIdBinding.Option("generatedId", rGenerated),
        IdempotencyKeyBinding.Option("idempotencyKey", rIdm)
      ) => (rAtomId, rInstrument, rStepConfig, rTelescopeConfig, rObserveClass, rGenerated, rIdm).parMapN:
        (atomId, instrument, step, telescope, oclass, generated, idm) =>
          RecordStepInput(atomId, instrument, step, telescope.getOrElse(TelescopeConfig.Default), oclass, generated, idm)

  val Flamingos2Binding: Matcher[RecordStepInput[Flamingos2DynamicConfig]] =
    binding("flamingos2", Flamingos2DynamicInput.Binding)

  val GmosNorthBinding: Matcher[RecordStepInput[GmosNorth]] =
    binding("gmosNorth", GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[RecordStepInput[GmosSouth]] =
    binding("gmosSouth", GmosSouthDynamicInput.Binding)