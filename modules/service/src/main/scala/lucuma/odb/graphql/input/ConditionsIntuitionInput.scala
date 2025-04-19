// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.Ior
import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

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

