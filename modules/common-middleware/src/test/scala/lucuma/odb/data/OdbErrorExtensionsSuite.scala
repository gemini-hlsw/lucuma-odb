// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.OdbErrorExtensions.*

class OdbErrorExtensionsSuite extends munit.FunSuite:

  test("asFailure carries the message and the odb_error extension"):
    OdbError.InvalidArgument("Too many nights.".some).asFailure match
      case Result.Failure(problems) =>
        val p = problems.head
        assertEquals(p.message, "Too many nights.")
        assert(p.extensions.exists(_.contains(OdbError.Key)))
      case other                    =>
        fail(s"Expected Result.Failure, got: $other")
