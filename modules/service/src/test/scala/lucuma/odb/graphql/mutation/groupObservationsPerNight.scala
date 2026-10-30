// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.service.ObsExtract
import lucuma.odb.util.Codecs.group_id
import skunk.Query
import skunk.codec.boolean.bool
import skunk.syntax.all.*

class groupObservationsPerNight extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

  private def limitsSet(min: Option[Int], max: Option[Int]): String =
    s"minimumObservationsPerNight: ${min.asJson} maximumObservationsPerNight: ${max.asJson}"

  private def createLimitedGroup(pid: Program.Id, min: Option[Int], max: Option[Int], parent: Option[Group.Id] = None): IO[Group.Id] =
    query(
      user  = pi,
      query = s"""
        mutation {
          createGroup(input: {
            programId: "$pid"
            SET: { ${limitsSet(min, max)} parentGroup: ${parent.asJson} }
          }) {
            group { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createGroup", "group", "id").require[Group.Id])

  private def setLimits(gid: Group.Id, min: Option[Int], max: Option[Int]): String =
    s"""
      mutation {
        updateGroups(input: {
          WHERE: { id: { EQ: "$gid" } }
          SET: { ${limitsSet(min, max)} }
        }) {
          groups { minimumObservationsPerNight maximumObservationsPerNight }
        }
      }
    """

  private def moveInto(gid: Group.Id, dest: Group.Id): String =
    s"""
      mutation {
        updateGroups(input: {
          WHERE: { id: { EQ: "$gid" } }
          SET: { parentGroup: "$dest" }
        }) {
          groups { id }
        }
      }
    """

  private def limits(gid: Group.Id): IO[Json] =
    query(
      user  = pi,
      query = s"""
        query {
          group(groupId: "$gid") { minimumObservationsPerNight maximumObservationsPerNight }
        }
      """
    ).map(_.hcursor.downField("group").focus.get)

  private def expectSubgroupError(q: String, gid: Group.Id): IO[Unit] =
    expectOdbError(
      user     = pi,
      query    = q,
      expected = {
        case OdbError.InconsistentGroupError(Some(msg)) =>
          assertEquals(msg, s"Group $gid has observations-per-night limits, so it cannot contain subgroups.")
      }
    )

  private def setExistence(gid: Group.Id, existence: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateGroups(input: { WHERE: { id: { EQ: "$gid" } } SET: { existence: $existence } }) { groups { id } }
        }
      """
    ).void

  // Turns `gid` into a per-observation calibration group, as the ODB creates
  // to hold a science observation with its tellurics.
  private def makeObsCalibrationGroup(gid: Group.Id): IO[Unit] =
    updateGroupSystem(gid, true) *> setGroupCalibrationRoles(gid, List(CalibrationRole.Telluric))

  test("create and read back"):
    for
      pid <- createProgramAs(pi)
      gid <- createLimitedGroup(pid, 2.some, 3.some)
      js  <- limits(gid)
    yield assertEquals(js, json"""{ "minimumObservationsPerNight": 2, "maximumObservationsPerNight": 3 }""")

  test("unset by default"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      js  <- limits(gid)
    yield assertEquals(js, json"""{ "minimumObservationsPerNight": null, "maximumObservationsPerNight": null }""")

  test("cannot create with zero"):
    createProgramAs(pi).flatMap: pid =>
      createLimitedGroup(pid, 0.some, None).attempt.map(r => assert(r.isLeft, s"Expected failure, got $r"))

  test("cannot create with the minimum above the maximum"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            createGroup(input: { programId: "$pid" SET: { ${limitsSet(3.some, 2.some)} } }) { group { id } }
          }
        """,
        expected = List(
          "Argument 'input.SET' is invalid: Minimum observations per night must be less than or equal maximum observations per night."
        ).asLeft
      )

  test("cannot create with same night"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            createGroup(input: { programId: "$pid" SET: { sameNight: true minimumObservationsPerNight: 2 } }) { group { id } }
          }
        """,
        expected = List(
          "Argument 'input.SET' is invalid: Same night and observations per night are mutually exclusive."
        ).asLeft
      )

  test("can create an OR group with limits"):
    for
      pid <- createProgramAs(pi)
      gid <- createLimitedGroup(pid, 2.some, None)
      _   <- expect(
               user     = pi,
               query    = s"""
                 mutation {
                   updateGroups(input: { WHERE: { id: { EQ: "$gid" } } SET: { minimumRequired: 3 } }) {
                     groups { minimumRequired minimumObservationsPerNight }
                   }
                 }
               """,
               expected = json"""
                 { "updateGroups": { "groups": [ { "minimumRequired": 3, "minimumObservationsPerNight": 2 } ] } }
               """.asRight
             )
    yield ()

  test("update and clear"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = setLimits(gid, 1.some, 4.some),
               expected = json"""
                 { "updateGroups": { "groups": [ { "minimumObservationsPerNight": 1, "maximumObservationsPerNight": 4 } ] } }
               """.asRight
             )
      _   <- expect(
               user     = pi,
               query    = setLimits(gid, None, None),
               expected = json"""
                 { "updateGroups": { "groups": [ { "minimumObservationsPerNight": null, "maximumObservationsPerNight": null } ] } }
               """.asRight
             )
    yield ()

  test("cannot update the minimum above the existing maximum"):
    for
      pid <- createProgramAs(pi)
      gid <- createLimitedGroup(pid, None, 2.some)
      _   <- expect(
               user     = pi,
               query    = s"""
                 mutation {
                   updateGroups(input: { WHERE: { id: { EQ: "$gid" } } SET: { minimumObservationsPerNight: 3 } }) {
                     groups { id }
                   }
                 }
               """,
               expected = List(
                 "Minimum observations per night must be less than or equal maximum observations per night."
               ).asLeft
             )
    yield ()

  test("cannot turn on same night with limits"):
    for
      pid <- createProgramAs(pi)
      gid <- createLimitedGroup(pid, 2.some, None)
      _   <- expect(
               user     = pi,
               query    = s"""
                 mutation {
                   updateGroups(input: { WHERE: { id: { EQ: "$gid" } } SET: { sameNight: true } }) {
                     groups { id }
                   }
                 }
               """,
               expected = List("Same night and observations per night are mutually exclusive.").asLeft
             )
    yield ()

  test("cannot set limits on a group with a subgroup"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- createGroupAs(pi, pid, parentGroupId = gid.some)
      _   <- expectSubgroupError(setLimits(gid, 2.some, None), gid)
    yield ()

  test("can set limits on a group whose subgroup is deleted"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      sub <- createGroupAs(pi, pid, parentGroupId = gid.some)
      _   <- setExistence(sub, "DELETED")
      _   <- query(pi, setLimits(gid, 2.some, None))
    yield ()

  test("cannot restore a deleted subgroup in a group with limits"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      sub <- createGroupAs(pi, pid, parentGroupId = gid.some)
      _   <- setExistence(sub, "DELETED")
      _   <- query(pi, setLimits(gid, 2.some, None))
      _   <- expectSubgroupError(
               s"""
                 mutation {
                   updateGroups(input: { WHERE: { id: { EQ: "$sub" } } SET: { existence: PRESENT } }) { groups { id } }
                 }
               """,
               gid
             )
    yield ()

  test("cannot move a group into a group with limits"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      gid  <- createGroupAs(pi, pid)
      _    <- expectSubgroupError(moveInto(gid, dest), dest)
    yield ()

  test("can move a group into a group with limits and delete it at once"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      gid  <- createGroupAs(pi, pid)
      _    <- query(
                user  = pi,
                query = s"""
                  mutation {
                    updateGroups(input: {
                      WHERE: { id: { EQ: "$gid" } }
                      SET: { parentGroup: "$dest" existence: DELETED }
                    }) {
                      groups { id }
                    }
                  }
                """
              )
    yield ()

  test("cannot create a group inside a group with limits"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      _    <- expectSubgroupError(
                s"""
                  mutation {
                    createGroup(input: { programId: "$pid" SET: { parentGroup: "$dest" } }) { group { id } }
                  }
                """,
                dest
              )
    yield ()

  test("cannot create a group with limits holding a subgroup"):
    for
      pid <- createProgramAs(pi)
      sub <- createGroupAs(pi, pid)
      _   <- expectOdbError(
               user     = pi,
               query    = s"""
                 mutation {
                   createGroup(input: {
                     programId: "$pid"
                     SET: { minimumObservationsPerNight: 2 }
                     initialContents: [ { groupId: "$sub" } ]
                   }) {
                     group { id }
                   }
                 }
               """,
               expected = {
                 case OdbError.InconsistentGroupError(Some(msg)) =>
                   assert(msg.endsWith("has observations-per-night limits, so it cannot contain subgroups."), msg)
               }
             )
    yield ()

  test("cannot clone a group into a group with limits"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      gid  <- createGroupAs(pi, pid)
      _    <- expectSubgroupError(
                s"""
                  mutation {
                    cloneGroup(input: { groupId: "$gid" SET: { parentGroup: "$dest" } }) { newGroup { id } }
                  }
                """,
                dest
              )
    yield ()

  test("can set limits on a group holding observations and a per-observation calibration group"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- createObservationInGroupAs(pi, pid, gid.some)
      cal <- createGroupAs(pi, pid, parentGroupId = gid.some)
      _   <- makeObsCalibrationGroup(cal)
      _   <- query(pi, setLimits(gid, 2.some, 2.some))
      js  <- limits(gid)
    yield assertEquals(js, json"""{ "minimumObservationsPerNight": 2, "maximumObservationsPerNight": 2 }""")

  test("can move a per-observation calibration group into a group with limits"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      cal  <- createGroupAs(pi, pid)
      _    <- makeObsCalibrationGroup(cal)
      _    <- query(pi, moveInto(cal, dest))
    yield ()

  test("cannot turn a per-observation calibration group in a group with limits into an ordinary group"):
    for
      pid  <- createProgramAs(pi)
      dest <- createLimitedGroup(pid, 2.some, None)
      cal  <- createGroupAs(pi, pid)
      _    <- makeObsCalibrationGroup(cal)
      _    <- query(pi, moveInto(cal, dest))
      r    <- setGroupCalibrationRoles(cal, Nil).attempt
    yield r match
      case Left(e)  => assert(e.getMessage.contains(s"Group $dest has observations-per-night limits"), e.getMessage)
      case Right(_) => fail("Expected the change to be rejected.")

  // The migration's `is_obs_calibration_group` hardcodes the per-observation
  // calibration roles, so a role added to the Scala list must be added there too.
  test("is_obs_calibration_group agrees with the per-observation calibration roles"):
    val IsObsCalibrationGroup: Query[Group.Id, Boolean] =
      sql"SELECT is_obs_calibration_group($group_id)".query(bool)
    for
      pid <- createProgramAs(pi)
      _   <- CalibrationRole.values.toList.traverse_ : role =>
               for
                 gid <- createGroupAs(pi, pid)
                 _   <- updateGroupSystem(gid, true) *> setGroupCalibrationRoles(gid, List(role))
                 is  <- withSession(_.unique(IsObsCalibrationGroup)(gid))
               yield assertEquals(is, ObsExtract.PerObservationCalibrationRoles.contains(role), role.toString)
    yield ()

  test("clone keeps the limits"):
    for
      pid <- createProgramAs(pi)
      gid <- createLimitedGroup(pid, 2.some, 5.some)
      cid <- cloneGroupAs(pi, gid)
      js  <- limits(cid)
    yield assertEquals(js, json"""{ "minimumObservationsPerNight": 2, "maximumObservationsPerNight": 5 }""")
