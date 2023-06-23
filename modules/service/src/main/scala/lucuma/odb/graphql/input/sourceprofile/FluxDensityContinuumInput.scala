// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Units
import lucuma.core.util.*
import lucuma.odb.graphql.binding._

object FluxDensityContinuumInput {

  object Integrated {
    val Binding: Matcher[FluxDensityContinuumMeasure[Integrated]] =
      binding(enumeratedBinding[Units Of FluxDensityContinuum[Integrated]])
  }

  object Surface {
    val Binding: Matcher[FluxDensityContinuumMeasure[Surface]] =
      binding(enumeratedBinding[Units Of FluxDensityContinuum[Surface]])
  }

  def binding[A](
    unitsBinding: Matcher[Units Of FluxDensityContinuum[A]]
  ): Matcher[FluxDensityContinuumMeasure[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        FluxDensityContinuumBinding("value", rValue),
        unitsBinding("units", rUnits),
        FluxDensityContinuumBinding.Option("error", rError),
      ) =>
        (rUnits, rValue, rError).mapN(_.withValueTagged(_, _))
    }

}
