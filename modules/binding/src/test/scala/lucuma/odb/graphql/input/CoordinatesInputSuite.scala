// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import grackle.Query.Binding
import grackle.Result
import grackle.Value
import grackle.Value.*
import munit.FunSuite

class CoordinatesInputSuite extends FunSuite {

  // RightAscensionInput.Binding's rmap pattern, in order: microseconds, degrees, hours, hms.
  def invalidRa: Value =
    ObjectValue(List(
      "microseconds" -> AbsentValue,
      "degrees"      -> AbsentValue,
      "hours"        -> AbsentValue,
      "hms"          -> AbsentValue
    ))

  // DeclinationInput.Binding's rmap pattern, in order: microarcseconds, degrees, dms.
  def invalidDec: Value =
    ObjectValue(List(
      "microarcseconds" -> AbsentValue,
      "degrees"         -> AbsentValue,
      "dms"             -> AbsentValue
    ))

  def messages[A](r: Result[A]): List[String] =
    r.toProblems.toList.map(_.message)

  test("Edit binding with an invalid ra and an invalid dec gives two problems, in field order") {
    // CoordinatesInput.Edit.Binding's rmap pattern, in order: ra, dec.
    val input = ObjectValue(List("ra" -> invalidRa, "dec" -> invalidDec))
    val r     = CoordinatesInput.Edit.Binding.validate(Binding("input", input))
    assertEquals(
      messages(r),
      List(
        "Argument 'input.ra' is invalid: Expected exactly one of microseconds, degrees, hours, hms",
        "Argument 'input.dec' is invalid: Expected exactly one of microarcseconds, degrees, dms"
      )
    )
  }

}
