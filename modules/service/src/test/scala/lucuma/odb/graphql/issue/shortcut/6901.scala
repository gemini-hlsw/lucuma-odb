// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import lucuma.core.model.Ephemeris

class ShortCut_6901 extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  test("create target with a user-defined ephemeris"):
    for
      pid <- createProgramAs(pi)
      eph <- createUserDefinedEphemerisFor(Ephemeris.Key.Comet("1P"))
      tid <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, pid, eph)
    yield ()

