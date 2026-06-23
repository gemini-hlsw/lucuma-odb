// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*
import skunk.syntax.all.*

class programObjectLimit extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers = List(pi)

  // Set a small per-program override so the cap is easy to reach.
  private def setLimit(pid: Program.Id, max: Int): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"update t_program set c_max_objects = ${int4} where c_program_id = $program_id".command
      )((max, pid)).void

  test("counts observations, groups, and targets together against the limit"):
    for
      pid <- createProgramAs(pi)
      _   <- setLimit(pid, 3)
      _   <- createTargetAs(pi, pid)        // 1
      _   <- createGroupAs(pi, pid)         // 2
      _   <- createObservationAs(pi, pid)   // 3 (at the limit)
      // The 4th object of any type should be rejected.
      _   <- interceptOdbError(createTargetAs(pi, pid)):
               case OdbError.ProgramObjectLimitExceeded(_) => ()
    yield ()

  test("soft-deleting an object frees capacity"):
    for
      pid <- createProgramAs(pi)
      _   <- setLimit(pid, 1)
      tid <- createTargetAs(pi, pid)        // at the limit
      _   <- interceptOdbError(createGroupAs(pi, pid)):
               case OdbError.ProgramObjectLimitExceeded(_) => ()
      _   <- deleteTargetAs(pi, tid)        // frees a slot
      _   <- createGroupAs(pi, pid)         // now succeeds
    yield ()