// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.math.Parallax
import lucuma.odb.graphql.binding.*

object ParallaxModelInput {

  val Binding: Matcher[Parallax] =
    OneOfBinding(
      "microarcseconds" -> LongBinding.map(Parallax.microarcseconds.reverseGet),
      "milliarcseconds" -> BigDecimalBinding.map(Parallax.milliarcseconds.reverseGet)
    )

}