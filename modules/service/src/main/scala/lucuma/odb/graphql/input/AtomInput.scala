// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep

object AtomInput:

  def binding[D](
    stepBinding: Matcher[ProtoStep[D]]
  ): Matcher[ProtoAtom[ProtoStep[D]]] =
    ObjectFieldsBinding.rmap:
      case List(
        NonEmptyStringBinding.Option("description", rDescription),
        stepBinding.List("steps", rSteps)
      ) => (rDescription, rSteps).parFlatMapN: (description, steps) =>
        NonEmptyList
          .fromList(steps)
          .fold(Matcher.validationFailure("At least one step is required in an atom.")): nel =>
            ProtoAtom(description, nel).success

  val Flamingos2Binding: Matcher[ProtoAtom[ProtoStep[Flamingos2DynamicConfig]]] =
    binding(StepInput.Flamingos2Binding)

  val GmosNorthBinding: Matcher[ProtoAtom[ProtoStep[GmosNorth]]] =
    binding(StepInput.GmosNorthBinding)

  val GmosSouthBinding: Matcher[ProtoAtom[ProtoStep[GmosSouth]]] =
    binding(StepInput.GmosSouthBinding)

  val Igrins2Binding: Matcher[ProtoAtom[ProtoStep[Igrins2DynamicConfig]]] =
    binding(StepInput.Igrins2Binding)
