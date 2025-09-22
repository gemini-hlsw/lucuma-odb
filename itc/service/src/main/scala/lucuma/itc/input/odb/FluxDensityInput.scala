// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.*

object FluxDensityInput:

  val Binding: Matcher[(Wavelength, BigDecimal)] =
    ObjectFieldsBinding.rmap:
      case List(
            WavelengthInput.Binding("wavelength", rWavelength),
            BigDecimalBinding("density", rDensity)
          ) =>
        (rWavelength, rDensity).parTupled
