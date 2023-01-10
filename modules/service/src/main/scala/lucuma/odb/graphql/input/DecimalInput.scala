// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.odb.graphql.binding._

/**
 * Constructor for matchers of the pattern:
 *
 * input FooDecimalnput {
 *   # integral value in associated units
 *   value: BigDecimal!
 *
 *   # units for associated value
 *   units: FooUnits!
 * }
 *
 */object DecimalInput {

  def apply[A](name: String)(handler: PartialFunction[(BigDecimal, String), Result[A]]): Matcher[A] =
    ObjectFieldsBinding.rmap {
      case List(
        BigDecimalBinding("value", rValue),
        TypedEnumBinding("units", rEnum)
      ) =>
        (rValue, rEnum.map(_.name)).parTupled.flatMap {
          case (value, label) =>
            handler.lift((value, label)) match {
              case Some(r) => r
              case None    => Result.failure(s"Unexpected ${name}Units value: $label")
            }
      }
    }

}
