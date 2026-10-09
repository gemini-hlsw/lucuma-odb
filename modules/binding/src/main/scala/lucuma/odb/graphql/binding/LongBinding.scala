// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Result
import grackle.Value
import grackle.syntax.*

import scala.util.control.NonFatal

val LongBinding: Matcher[Long] = {
  case Value.IntValue(v)            => v.toLong.success
  case v @ Value.FloatValue(double) =>
    lazy val long = double.toLong
    if double.isWhole && long.toDouble == double then long.success
    else Result.failure(s"Expected Long, got $v")
  case Value.StringValue(v)         =>
    try v.toLong.success
    catch { case NonFatal(e) => Result.failure(s"Invalid Long: $v: ${e.getMessage}") }
  case Value.NullValue              => Result.failure(s"cannot be null")
  case Value.AbsentValue            => Result.failure(s"cannot be absent")
  case other                        => Result.failure(s"Expected Long, got $other")
}
