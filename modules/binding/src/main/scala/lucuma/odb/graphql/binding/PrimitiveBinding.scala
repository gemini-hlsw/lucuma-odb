// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Value
import grackle.Value.AbsentValue
import grackle.Value.NullValue

/** A primitive non-nullable binding. */
def primitiveBinding[A](name: String)(pf: PartialFunction[Value, A]): Matcher[A] =
  case NullValue   => Left(s"$name cannot be null")
  case AbsentValue => Left(s"$name is not optional")
  case other       =>
    pf.lift(other) match {
      case Some(value) => Right(value)
      case None        => Left(s"expected $name, found $other")
    }
