// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.units._
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

object LineWidthInput {
  val Binding: Matcher[Quantity[PosBigDecimal, KilometersPerSecond]] =
    BigDecimalBinding.emap { d =>
      PosBigDecimal.from(d) match {
        case Left(msg)    => Left(msg)
        case Right(value) => Right(Quantity(value))
      }
    }
}