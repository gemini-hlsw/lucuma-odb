// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.OdbErrorExtensions.*

class OdbErrorExtensionsSuite extends munit.FunSuite:

  test("asFailure carries the message and round-trips the error through the odb_error extension"):
    val err = OdbError.InvalidArgument("Too many nights.".some)
    err.asFailure match
      case Result.Failure(problems) =>
        assertEquals(problems.length, 1L)
        val p = problems.head
        assertEquals(p.message, "Too many nights.")
        assertEquals(p.extensions.flatMap(_(OdbError.Key)).map(_.as[OdbError]), err.asRight.some)
      case other                    =>
        fail(s"Expected Result.Failure, got: $other")
