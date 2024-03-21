// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import coulomb.Quantity
import lucuma.core.math.BrightnessUnits.LineWidthQuantity
import lucuma.core.math.LineWidthValue
import lucuma.core.math.units.*
import lucuma.odb.graphql.binding.*

val LineWidthBinding: Matcher[LineWidthQuantity] =
  BigDecimalBinding.emap { d =>
    LineWidthValue.from(d).map { lwv =>
      Quantity[KilometersPerSecond](lwv)
    }
  }
