// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*
import skunk.syntax.all.*

class programResourceLimit extends OdbSuite:

  val pi    = TestUsers.Standard.pi(nextId, nextId)
  val staff = TestUsers.Standard.staff(nextId, nextId)

  lazy val validUsers = List(pi, staff)

  private def setResourceLimitAs(user: User, pid: Program.Id, limit: Int): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          setProgramResourceLimit(input: { programId: "$pid", limit: $limit }) {
            program { id }
          }
        }
      """
    ).void

  // Attachments have no GraphQL creation helper (they go through an HTTP upload
  // route), so insert one directly to exercise attachment counting.
  private def insertAttachment(pid: Program.Id, fileName: String): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          insert into t_attachment (c_program_id, c_attachment_type, c_file_name, c_file_size, c_remote_path)
          values ($program_id, 'finder', $text, 0, $text)
        """.command
      )((pid, fileName, s"remote/$fileName")).void

  test("observations, groups, targets, and program notes count together"):
    for
      pid <- createProgramAs(pi)
      _   <- setResourceLimitAs(staff, pid, 4)
      _   <- createTargetAs(pi, pid)                          // 1
      _   <- createGroupAs(pi, pid)                           // 2
      _   <- createObservationAs(pi, pid)                     // 3
      _   <- createProgramNoteAs(pi, pid, "note", "x".some)   // 4 (at the limit)
      _   <- interceptOdbError(createTargetAs(pi, pid)):
               case OdbError.ProgramResourceLimitExceeded(_) => ()
    yield ()

  test("attachments count toward the limit"):
    for
      pid <- createProgramAs(pi)
      _   <- setResourceLimitAs(staff, pid, 1)
      _   <- insertAttachment(pid, "finder.png")   // 1 (at the limit)
      _   <- interceptOdbError(createTargetAs(pi, pid)):
               case OdbError.ProgramResourceLimitExceeded(_) => ()
    yield ()

  test("soft-deleting a resource frees capacity"):
    for
      pid <- createProgramAs(pi)
      _   <- setResourceLimitAs(staff, pid, 1)
      tid <- createTargetAs(pi, pid)               // at the limit
      _   <- interceptOdbError(createGroupAs(pi, pid)):
               case OdbError.ProgramResourceLimitExceeded(_) => ()
      _   <- deleteTargetAs(pi, tid)               // frees a slot
      _   <- createGroupAs(pi, pid)                // now succeeds
    yield ()

  test("resourceLimit and resourceCount are readable on Program (no staff gating)"):
    for
      pid <- createProgramAs(pi)
      _   <- createTargetAs(pi, pid)
      _   <- createGroupAs(pi, pid)
      _   <- expect(
               user  = pi,
               query = s"""query { program(programId: "$pid") { resourceLimit resourceCount } }""",
               expected = Right(json"""
                 { "program": { "resourceLimit": 1000, "resourceCount": 2 } }
               """)
             )
    yield ()

  test("only staff may set the resource limit"):
    for
      pid <- createProgramAs(pi)
      _   <- expectOdbError(
               user  = pi,
               query = s"""
                 mutation {
                   setProgramResourceLimit(input: { programId: "$pid", limit: 50 }) {
                     program { id }
                   }
                 }
               """,
               expected = { case OdbError.NotAuthorized(_, _) => () }
             )
      _   <- expect(
               user  = staff,
               query = s"""
                 mutation {
                   setProgramResourceLimit(input: { programId: "$pid", limit: 5000 }) {
                     program { resourceLimit }
                   }
                 }
               """,
               expected = Right(json"""
                 { "setProgramResourceLimit": { "program": { "resourceLimit": 5000 } } }
               """)
             )
    yield ()
