// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Result
import grackle.Value
import grackle.syntax.*

import scala.util.control.NonFatal

val BigDecimalBinding: Matcher[BigDecimal] = {
  case Value.IntValue(v)    => BigDecimal(v).success
  case Value.FloatValue(v)  => BigDecimal(v).success
  case Value.StringValue(v) =>
    try BigDecimal(v).success
    catch { case NonFatal(e) => Result.failure(s"Invalid BigDecimal: $v: ${e.getMessage}") }
  case Value.NullValue      => Result.failure(s"cannot be null")
  case Value.AbsentValue    => Result.failure(s"cannot be absent")
  case other                => Result.failure(s"Expected BigDecimal, got $other")
}
