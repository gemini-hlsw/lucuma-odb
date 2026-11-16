// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.group.GroupConstraintViolation.Waivable
import lucuma.odb.graphql.binding.*
import lucuma.odb.syntax.groupConstraintViolation.*

/**
 * A waivable violation of a group's constraints, identified by its code and
 * the element or night where it happened.  A code that can't be waived, or a
 * location that doesn't match the code, is rejected.
 */
object GroupConstraintViolationInput:

  val Binding: Matcher[Waivable] =
    ObjectFieldsBinding.rmap:
      case List(
        GroupConstraintViolationCodeBinding("code", rCode),
        GroupElementInput.Binding.Option("element", rElement),
        ObservingNightInput.Binding.Option("night", rNight)
      ) =>
        // Whether the code can be waived depends on the code alone, so it is
        // checked before, and reported along with, the location.
        val rCodeʹ = rCode.flatMap(_.checkWaivable.fold(Result.failure, Result(_)))
        (rCodeʹ, rElement, rNight).parFlatMapN: (code, element, night) =>
          Waivable.fromLocation(code, element.map(_.value), night).fold(Result.failure, Result(_))
