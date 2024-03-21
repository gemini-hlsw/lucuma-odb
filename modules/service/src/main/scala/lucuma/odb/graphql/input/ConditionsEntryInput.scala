// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.Ior
import cats.syntax.all.*
import grackle.Result
import lucuma.odb.graphql.binding.*

final case class ConditionsEntryInput(
  value: Ior[ConditionsMeasurementInput, ConditionsIntuitionInput]
)

object ConditionsEntryInput {

  val Binding: Matcher[ConditionsEntryInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ConditionsMeasurementInput.Binding.Option("measurement", rMeasurement),
        ConditionsIntuitionInput.Binding.Option("intuition", rIntuition),
      ) =>
        (rMeasurement, rIntuition).parTupled.flatMap { (o1, o2) =>
          Result.fromOption(
            Ior.fromOptions(o1, o2).map(ConditionsEntryInput.apply), 
            "At least one of measurement and intuition must be specified."
          )
        }
    }

}