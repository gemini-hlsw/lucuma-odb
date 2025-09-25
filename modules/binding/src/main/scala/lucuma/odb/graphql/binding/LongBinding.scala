// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all.*
import grackle.Value

import scala.util.control.NonFatal

val LongBinding: Matcher[Long] = {
  case Value.IntValue(v)            => v.toLong.asRight
  case v @ Value.FloatValue(double) =>
    lazy val long = double.toLong
    if double.isWhole && long.toDouble == double then long.asRight
    else s"Expected Long, got $v".asLeft
  case Value.StringValue(v)         =>
    try v.toLong.asRight
    catch { case NonFatal(e) => s"Invalid Long: $v: ${e.getMessage}".asLeft }
  case Value.NullValue              => s"cannot be null".asLeft
  case Value.AbsentValue            => s"cannot be absent".asLeft
  case other                        => s"Expected Long, got $other".asLeft
}
