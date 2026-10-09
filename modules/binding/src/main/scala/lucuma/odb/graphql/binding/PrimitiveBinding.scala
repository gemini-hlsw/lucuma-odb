// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Result
import grackle.Value
import grackle.Value.AbsentValue
import grackle.Value.NullValue
import grackle.syntax.*

/** A primitive non-nullable binding. */
def primitiveBinding[A](name: String)(pf: PartialFunction[Value, A]): Matcher[A] =
  case NullValue   => Result.failure(s"$name cannot be null")
  case AbsentValue => Result.failure(s"$name is not optional")
  case other       => pf.lift(other).toResult(s"expected $name, found $other")
