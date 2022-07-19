package lucuma.odb.graphql.snippet
package input
package sourceprofile

import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.units._
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