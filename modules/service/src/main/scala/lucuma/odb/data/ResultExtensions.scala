// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.either.*
import grackle.Result

import OdbErrorExtensions.*

object ResultExtensions:

  extension (self: Result.type)
    def fromEitherOdbError[A](e: Either[OdbError, A]): Result[A] =
      Result.fromEither(e.leftMap(_.asProblem))