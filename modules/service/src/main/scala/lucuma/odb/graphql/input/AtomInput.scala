// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class AtomInput[D](
  description: Option[NonEmptyString],
  steps:       List[StepInput[D]]
)

object AtomInput:

  def binding[D](
    stepBinding: Matcher[StepInput[D]]
  ): Matcher[AtomInput[D]] =
    ObjectFieldsBinding.rmap:
      case List(
        NonEmptyStringBinding.Option("description", rDescription),
        stepBinding.List("steps", rSteps)
      ) => (rDescription, rSteps).parMapN: (description, steps) =>
          AtomInput(description, steps)

  val Flamingos2Binding: Matcher[AtomInput[Flamingos2DynamicConfig]] =
    binding(StepInput.Flamingos2Binding)

  val GmosNorthBinding: Matcher[AtomInput[GmosNorth]] =
    binding(StepInput.GmosNorthBinding)

  val GmosSouthBinding: Matcher[AtomInput[GmosSouth]] =
    binding(StepInput.GmosSouthBinding)