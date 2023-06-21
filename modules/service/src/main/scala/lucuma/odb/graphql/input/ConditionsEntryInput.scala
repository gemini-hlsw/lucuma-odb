// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._
import cats.data.Ior
import edu.gemini.grackle.Result

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