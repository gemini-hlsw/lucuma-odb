// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import lucuma.core.enums.ProgramUserRole
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers

class ShortCut_6006 extends OdbSuite:

  val bob, andy = TestUsers.Standard.pi(nextId, nextId)
  val admin = TestUsers.Standard.admin(nextId, nextId)
  val validUsers = List(bob, andy, admin)

  test("Secondary support user should be able to clone an observation."):
    for 
      _   <- createUsers(andy)
      pid <- createProgramAs(bob)
      mid <- addProgramUserAs(admin, pid, ProgramUserRole.SupportSecondary)
      _   <- linkUserAs(admin, mid, andy.id)
      oid <- createObservationAs(bob, pid)
      _   <- cloneObservationAs(andy, oid) // was failing, should work now
    yield ()

