// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.math.Offset
import lucuma.odb.graphql.binding.*

object OffsetInput {

  val Binding: Matcher[Offset] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetComponentInput.BindingP("p", rP),
        OffsetComponentInput.BindingQ("q", rQ)
      ) => (rP, rQ).parMapN(Offset(_, _))
    }

}