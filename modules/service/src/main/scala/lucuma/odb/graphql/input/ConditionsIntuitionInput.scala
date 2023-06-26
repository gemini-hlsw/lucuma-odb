// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._

final case class ConditionsIntuitionInput(
   value: Ior[ConditionsExpectationInput, Tag]
)

object ConditionsIntuitionInput:

   val Binding: Matcher[ConditionsIntuitionInput] =
      ObjectFieldsBinding.rmap {
         case List(
            ConditionsExpectationInput.Binding.Option("expectation", rExpectation),
            TagBinding.Option("seeingTrend", rSeeingTrend)
         ) =>
            (rExpectation, rSeeingTrend).parTupled.flatMap { case (o1, o2) =>
               Result.fromOption(
                  Ior.fromOptions(o1, o2).map(ConditionsIntuitionInput.apply),
                  "At least one of expectation and seeingTrend must be specified."
               )
            }
      }

