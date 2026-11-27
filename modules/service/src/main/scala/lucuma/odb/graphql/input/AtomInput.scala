// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.ProtoAtom

object AtomInput:

  def binding[D](
    stepBinding: Matcher[StepInput.Spec[D]]
  ): Matcher[ProtoAtom[StepInput.Spec[D]]] =
    ObjectFieldsBinding.rmap:
      case List(
        NonEmptyStringBinding.Option("description", rDescription),
        stepBinding.List("steps", rSteps)
      ) => 
        (
          rDescription, 
          rSteps.flatMap(NonEmptyList.fromList(_).toResult("At least one step is required in an atom."))
        ).parMapN(ProtoAtom.apply)

  val Flamingos2Binding: Matcher[ProtoAtom[StepInput.Spec[Flamingos2DynamicConfig]]] =
    binding(StepInput.Flamingos2Binding)

  val GmosNorthBinding: Matcher[ProtoAtom[StepInput.Spec[GmosNorth]]] =
    binding(StepInput.GmosNorthBinding)

  val GmosSouthBinding: Matcher[ProtoAtom[StepInput.Spec[GmosSouth]]] =
    binding(StepInput.GmosSouthBinding)

  val Igrins2Binding: Matcher[ProtoAtom[StepInput.Spec[Igrins2DynamicConfig]]] =
    binding(StepInput.Igrins2Binding)

  val GnirsBinding: Matcher[ProtoAtom[StepInput.Spec[GnirsDynamicConfig]]] =
    binding(StepInput.GnirsBinding)

  val GhostBinding: Matcher[ProtoAtom[StepInput.Spec[GhostDynamicConfig]]] =
    binding(StepInput.GhostBinding)
