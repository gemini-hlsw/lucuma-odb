// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gmos.GmosGratingConfig.South
import lucuma.odb.graphql.binding.*

object GmosSouthGratingConfigInput {

  val Binding: Matcher[South] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosSouthGratingBinding("grating", rGrating),
        GmosGratingOrderBinding("order", rOrder),
        WavelengthInput.Binding("wavelength", rWavelength)
      ) => (rGrating, rOrder, rWavelength).parMapN { (grating, order, wavelength) =>
        South(grating, order, wavelength)
      }
    }

}