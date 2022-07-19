package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import cats.syntax.all._
import edu.gemini.grackle.Result

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
