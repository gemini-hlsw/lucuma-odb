// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.traverse.*
import grackle.Result
import io.circe.ACursor
import io.circe.Json
import lucuma.core.enums.ObservationValidationCode

/**
 * The `ObservationValidationCodeSeverity` of `ObservationValidation` and
 * `StepViolation` is an API field alone, determined by the code.  It is added
 * to their JSON as it is served rather than stored.
 */
object ValidationSeverity:

  def severity(code: ObservationValidationCode): Json =
    Json.fromString(code.fold(_ => "ERROR", _ => "WARNING"))

  /** Adds the `severity` to a JSON object with an `ObservationValidationCode` `code`. */
  def add(json: Json): Result[Json] =
    json.hcursor.downField("code").as[ObservationValidationCode].fold(
      f => Result.internalError(s"Could not determine the validation severity: ${f.message}"),
      c => Result(json.mapObject(_.add("severity", severity(c))))
    )

  /** Adds the `severity` to each of a JSON array of objects with a `code`. */
  def addAll(json: Json): Result[Json] =
    json.asArray.fold(Result.internalError(s"Expected an array of validations, not: ${json.noSpaces}")):
      _.traverse(add).map(Json.fromValues)

  /** Adds the `severity` to each element of the array at `c`, if there is one. */
  def addAllAt(json: Json, c: ACursor): Result[Json] =
    c.focus.fold(Result(json)): vs =>
      addAll(vs).map(c.set(_).top.getOrElse(json))
