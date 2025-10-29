// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut
import cats.effect.Deferred
import cats.effect.IO
import grackle.Result
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.odb.service.Services.Syntax.*

class ShortCut_7118 extends OdbSuite with DatabaseOperations:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val svc = TestUsers.service(nextId)

  val validUsers = List(pi, svc)

  def deleteSystemGroup(pid: Program.Id, gid: Group.Id): IO[Result[Unit]] =
    withServices(svc): ss =>
      ss.transactionally:
        groupService(null, null).deleteSystemGroup(pid, gid)

  def assertFailure(a: Result[Any], msg: String): Unit =
    a match
      case Result.Failure(ps) => assertEquals(ps.head.message, msg)
      case _ => fail(s"Expected failure ($msg), found $a")

  test("can't delete non-system group"):
    for
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      res <- deleteSystemGroup(pid, gid)
    yield assertFailure(res, s"Cannot delete non-sytem group $gid.")

  test("delete empty system group at top level"):
    for
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      _   <- updateGroupSystem(gid, true)
      _   <- assertIO(deleteSystemGroup(pid, gid), Result.unit)
    yield (pid, gid)

  test("delete empty system group at non-top level"):
    for
      pid <- createProgramAs(pi)
      pg  <- createGroupAs(pi, pid) // parent group
      _   <- createObservationInGroupAs(pi, pid, Some(pg), None)
      gid <- createGroupAs(pi, pid, Some(pg), None) // make sure group is in the middle
      _   <- createObservationInGroupAs(pi, pid, Some(pg), None)
      _   <- updateGroupSystem(gid, true)
      _   <- assertIO(deleteSystemGroup(pid, gid), Result.unit)
    yield (pid, gid)

  test("delete non-empty system group at top level"):
    for
      _   <- createUsers(svc) // make sure service user is in the database
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      _   <- updateGroupSystem(gid, true)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, tid)
      _   <- moveObservationAs(pi, oid, Some(gid))
      _   <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
      g2  <- createGroupAs(pi, pid, Some(gid))
      _   <- updateGroupSystem(g2, true)
      _   <- assertIO(deleteSystemGroup(pid, gid), Result.unit)
    yield ()

  test("delete non-empty system group at non-top level"):
    for
      _   <- createUsers(svc) // make sure service user is in the database
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, tid)
      _   <- moveObservationAs(pi, oid, Some(gid))
      _   <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
      g2  <- createGroupAs(pi, pid, Some(gid))
      _   <- updateGroupSystem(g2, true)
      _   <- assertIO(deleteSystemGroup(pid, g2), Result.unit)
    yield ()

  test("can't delete system group that contains a non-calibration observation"):
    for
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      _   <- updateGroupSystem(gid, true)
      _   <- createObservationInGroupAs(pi, pid, Some(gid), None) // not a calibration!
      res <- deleteSystemGroup(pid, gid)
    yield assertFailure(res, "One or more specified observations are not calibrations.")

  test("can't delete system group that contains a non-system group"):
    for
      _   <- createUsers(svc) // make sure service user is in the database
      pid <- createProgramAs(pi)
      _   <- createObservationAs(pi, pid)
      gid <- createGroupAs(pi, pid) // make sure group is in the middle
      _   <- createObservationAs(pi, pid)
      _   <- updateGroupSystem(gid, true)
      tid <- createTargetAs(pi, pid)
      oid <- createObservationAs(pi, pid, tid)
      _   <- moveObservationAs(pi, oid, Some(gid))
      _   <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
      g2  <- createGroupAs(pi, pid, Some(gid)) // not a system group
      r   <- deleteSystemGroup(pid, gid)
    yield assertFailure(r, s"Cannot delete non-sytem group $g2.")

  test("event (should trigger on hard delete)"):
    Deferred[IO, (Program.Id, Group.Id)].flatMap: d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              editType
            }
          }
        """,
        mutations =
          Right:
            for
              pid <- createProgramAs(pi)
              gid <- createGroupAs(pi, pid) // make sure group is in the middle
              _   <- d.complete((pid, gid))
              _   <- updateGroupSystem(gid, true)
              _   <- deleteSystemGroup(pid, gid)
            yield (),
        expectedF =
          d.get.map: (_, _) =>
            List(
              json"""{
                "groupEdit" : {
                  "editType" : "CREATED"
                }
              }""",
              json"""{
                "groupEdit" : {
                  "editType" : "UPDATED"
                }
              }""",
              json"""{
                "groupEdit" : {
                  "editType" : "UPDATED"
                }
              }""",
              json"""{
                "groupEdit" : {
                  "editType" : "HARD_DELETE"
                }
              }"""
          )
      )
