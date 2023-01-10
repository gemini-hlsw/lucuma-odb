// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all._
import edu.gemini.grackle.Value

import scala.util.control.NonFatal

val LongBinding: Matcher[Long] = {
  case Value.IntValue(v)     => v.toLong.asRight
  case Value.StringValue(v)  =>
    try v.toLong.asRight
    catch { case NonFatal(e) => s"Invalid Long: $v: ${e.getMessage}".asLeft }
  case Value.NullValue       => s"cannot be null".asLeft
  case Value.AbsentValue     => s"cannot be absent".asLeft
  case other                 => s"Expected Long, got $other".asLeft
}
