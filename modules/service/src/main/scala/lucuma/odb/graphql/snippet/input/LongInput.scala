package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import cats.syntax.all._
import edu.gemini.grackle.Result

/**
 * Constructor for matchers of the pattern:
 *
 * input FooLongInput {
 *   # integral value in associated units
 *   value: Long!
 *
 *   # units for associated value
 *   units: FooUnits!
 * }
 *
 */
object LongInput {

  def apply[A](enumName: String)(handler: PartialFunction[(Long, String), Result[A]]): Matcher[A] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding("value", rValue),
        TypedEnumBinding("units", rEnum)
      ) =>
        (rValue, rEnum.map(_.name)).parTupled.flatMap {
          case (value, label) =>
            handler.lift((value, label)) match {
              case Some(r) => r
              case None    => Result.failure(s"Unexpected ${enumName} value: $label")
            }
      }
    }

}
