// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import edu.gemini.grackle.Result
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.graphql.binding.*

object StepConfigInput {

  val Binding: Matcher[StepConfig] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("bias", rBias),
        BooleanBinding.Option("dark", rDark),
        StepConfigGcalInput.Binding.Option("gcal", rGcal),
        StepConfigScienceInput.Binding.Option("science", rScience),
        StepConfigSmartGcalInput.Binding.Option("smartGcal", rSmartGcal)
      ) => (rBias, rDark, rGcal, rScience, rSmartGcal).parTupled.flatMap {
         case (bias, dark, gcal, science, smartGcal) =>
           oneOrFail(
             bias.ifM(StepConfig.Bias.some, none) -> "bias",
             dark.ifM(StepConfig.Dark.some, none) -> "dark",
             gcal                                 -> "gcal",
             science                              -> "science",
             smartGcal                            -> "smartGcal"
           )

      }
    }

}
