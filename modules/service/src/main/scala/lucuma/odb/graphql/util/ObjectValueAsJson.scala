// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.syntax.all._
import edu.gemini.grackle.Value
import edu.gemini.grackle.Value._
import io.circe.Json

/** Extractor that provides in input value as JSON, if possible. */
object ValueAsJson {

  /**
   * Input value as JSON, when possible. `TypedEnumValue` and `IDValue` are represented as strings.
   * The following have no JSON representation:
   * - `AbsentValue`, except when it appears in an object field
   * - `FloatValue`, when its value is `NaN` or infinite
   * - `UntypedVariableName`
   * - `UntypedEnumValue`
   */
  def toJson(v: Value): Either[String, Json] = {
    def go(v: Value, position: String): Either[String, Json] = {
      lazy val err = s"No JSON representation for $v at $position"
      v match {

        // Easy
        case BooleanValue(value)   => Right(Json.fromBoolean(value))
        case NullValue             => Right(Json.Null)
        case IntValue(value)       => Right(Json.fromInt(value))
        case TypedEnumValue(value) => Right(Json.fromString(value.name))
        case IDValue(value)        => Right(Json.fromString(value))
        case StringValue(value)    => Right(Json.fromString(value))
        case FloatValue(value)     => Json.fromDouble(value).toRight(err)

        // Traverse lists with updated position
        case ListValue(elems)      =>
          elems
            .zipWithIndex
            .parTraverse { case (e, n) => go(e, s"$position[$n]") }
            .map(Json.fromValues)

        // Filter out AbsentValue fields and traverse remainder with update position
        case ObjectValue(fields) =>
          fields
            .filterNot(_._2 == AbsentValue)
            .parTraverse { case (k, v) =>
              go(v, s"$position/$k").map(k -> _)
            }
            .map(Json.fromFields)

        // These are always invalid
        case AbsentValue             |
             UntypedVariableValue(_) |
             UntypedEnumValue(_)     => Left(err)

      }
    }
    go(v, "<root>")
  }

}
