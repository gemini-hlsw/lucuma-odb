// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import lucuma.odb.graphql.binding.*

case class ImagingInput(
  asterism:    List[TargetDataInput],
  constraints: ItcConstraintsInput,
  mode:        InstrumentModesInput
)

object ImagingInput:

  val Binding: Matcher[ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(
            TargetDataInput.Binding.List("asterism", asterism),
            ItcConstraintsInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding("mode", mode)
          ) =>
        (asterism, constraints, mode)
          .parMapN(apply)
