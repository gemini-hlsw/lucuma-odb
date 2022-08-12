// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import lucuma.core.math.ProperMotion
import lucuma.odb.graphql.util.Bindings._

object ProperMotionInput {

  val Binding: Matcher[ProperMotion] =
    ObjectFieldsBinding.rmap {
      case List(
        ProperMotionComponentInput.RA.Binding("ra", rRa),
        ProperMotionComponentInput.Dec.Binding("dec", rDec)
      ) =>
        (rRa, rDec).parMapN(ProperMotion.apply)
    }

}