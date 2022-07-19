package lucuma.odb.graphql.snippet
package binding

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.Angle

object AngleBinding {

  val Microarcseconds: Matcher[Angle] =
    LongBinding.map(Angle.fromMicroarcseconds)

  val Degrees: Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromDoubleDegrees(bd.toDouble))

  val Dms: Matcher[Angle] =
    StringBinding.emap { s =>
      Angle.fromStringDMS.getOption(s).toRight(s"Invalid angle: $s")
    }

}
