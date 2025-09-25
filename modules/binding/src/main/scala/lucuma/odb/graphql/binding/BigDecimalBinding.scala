// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all.*
import grackle.Value

import scala.util.control.NonFatal

val BigDecimalBinding: Matcher[BigDecimal] = {
  case Value.IntValue(v)    => BigDecimal(v).asRight
  case Value.FloatValue(v)  => BigDecimal(v).asRight
  case Value.StringValue(v) =>
    try BigDecimal(v).asRight
    catch { case NonFatal(e) => s"Invalid BigDecimal: $v: ${e.getMessage}".asLeft }
  case Value.NullValue      => s"cannot be null".asLeft
  case Value.AbsentValue    => s"cannot be absent".asLeft
  case other                => s"Expected BigDecimal, got $other".asLeft
}
