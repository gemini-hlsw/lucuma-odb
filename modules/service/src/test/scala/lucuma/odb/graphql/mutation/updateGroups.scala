// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Existence
import lucuma.odb.data.OdbError

class updateGroups extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val service  = TestUsers.service(6)
  override lazy val validUsers: List[User] = List(pi, service)

  val inputIntervalError =
    "Argument 'input.SET' is invalid: Minimum interval must be less than or equal maximum interval."
  val odbIntervalError =
    "Minimum interval must be less than or equal maximum interval."

  test("simple bulk update") {
    for {
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      g3  <- createGroupAs(pi, pid) // not edited, just to be sure WHERE works
      _   <-
        expect(
          user = pi,
          query = s"""
            mutation {
              updateGroups(
                input: {
                  WHERE: {
                    id: {
                      IN: ${List(g1, g2).asJson}
                    }
                  }
                  SET: {
                    name: "foo"
                    description: "bar"
                    ordered: false
                    minimumRequired: 4
                    minimumInterval: { hours: 3 }
                    maximumInterval: { hours: 55 }
                  }
                }
              ) {
                groups {
                  id
                  name
                  description
                  ordered
                  minimumRequired
                  minimumInterval { minutes }
                  maximumInterval { minutes }
                }
              }
            }
          """,
          expected = Right(json"""
            {
              "updateGroups" : {
                "groups" : [
                  {
                    "id" : $g1,
                    "name" : "foo",
                    "description": "bar",
                    "ordered": false,
                    "minimumRequired": 4,
                    "minimumInterval" : {
                      "minutes" : 180.000000
                    },
                    "maximumInterval" : {
                      "minutes" : 3300.000000
                    }
                  },
                  {
                    "id" : $g2,
                    "name" : "foo",
                    "description": "bar",
                    "ordered": false,
                    "minimumRequired": 4,
                    "minimumInterval" : {
                      "minutes" : 180.000000
                    },
                    "maximumInterval" : {
                      "minutes" : 3300.000000
                    }
                  }
                ]
              }
            }
          """)
        )
    } yield ()
  }

  def moveGroupsAs(user: User, gids: List[Group.Id], gid: Option[Group.Id], index: Option[NonNegShort]): IO[Unit] =
    query(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              parentGroup: ${gid.asJson}
              ${index.map(_.value.asJson).foldMap(j => s"parentGroupIndex: $j")}
            },
            WHERE: {
              id: { IN: ${gids.asJson} }
            }
          }) {
            groups {
              id
            }
          }
        }
      """
    ).void

  private def gidElementSet(gs: Group.Id*): Set[Either[Group.Id, Observation.Id]] =
    gs.map(_.asLeft[Observation.Id]).toSet

  test("move groups into a group (at end)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, None, None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o1, o2), Some(gid), None)
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, gidElementSet(o2, o1, o3))
      assertEquals(es.drop(1).toSet, gidElementSet(o2, o1))
    }
  }

  test("move groups into a group (at beginning)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, None, None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, gidElementSet(o2, o1, o3))
      assertEquals(es.dropRight(1).toSet, gidElementSet(o2, o1))
    }
  }

  test("move groups into a group (in the middle)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, None, None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      o4  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o1, o2), Some(gid), Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, Some(gid))
    } yield {
      assertEquals(es.toSet, gidElementSet(o2, o1, o3, o4))
      assertEquals(es.drop(1).dropRight(1).toSet, gidElementSet(o2, o1))
    }
  }

  test("move groups out of a group (at end of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, Some(gid), None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o2, o3), None, None)
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2), List(Left(gid), Left(o1)))
      assertEquals(es.drop(2).toSet, gidElementSet(o2, o3))
    }
  }

  test("move groups out of a group (at beginning of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, Some(gid), None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o2, o3), None, Some(NonNegShort.unsafeFrom(0)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es.take(2).toSet, gidElementSet(o2, o3))
      assertEquals(es.drop(2), List(Left(gid), Left(o1)))
    }
  }

  test("move groups out of a group (in the middle of program)") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, None, None)
      o2  <- createGroupAs(pi, pid, Some(gid), None)
      o3  <- createGroupAs(pi, pid, Some(gid), None)
      _   <- moveGroupsAs(pi, List(o2, o3), None, Some(NonNegShort.unsafeFrom(1)))
      es  <- groupElementsAs(pi, pid, None)
    } yield {
      assertEquals(es(0), Left(gid))
      assertEquals(es.drop(1).take(2).toSet, gidElementSet(o2, o3))
      assertEquals(es(3), Left(o1))
    }
  }

  test("move groups between groups") {
    for {
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      o1  <- createGroupAs(pi, pid, Some(g1), None)
      o2  <- createGroupAs(pi, pid, Some(g1), None)
      o3  <- createGroupAs(pi, pid, Some(g1), None)
      _   <- moveGroupsAs(pi, List(o2, o3), Some(g2), None)
      e1  <- groupElementsAs(pi, pid, Some(g1))
      e2  <- groupElementsAs(pi, pid, Some(g2))
    } yield {
      assertEquals(e1, List(Left(o1)))
      assertEquals(e2.toSet, gidElementSet(o2, o3))
    }
  }

  test("Hugo's example, with groups") {
    for {
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      g3  <- createGroupAs(pi, pid)
      _   <- moveGroupsAs(pi, List(g1), Some(g3), None)
    } yield ()
  }

  def updateGroupExistenceAs(user: User, gid: Group.Id, existence: Existence): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              existence: ${existence.tag.toUpperCase}
            },
            WHERE: {
              id: { IN: [${gid.asJson}] }
            }
          }) {
            groups {
              id
              existence
            }
          }
        }
      """,
      expected = Right(json"""
        {
          "updateGroups": {
            "groups": [
              {
                "id": $gid,
                "existence": ${existence.asJson}
              }
            ]
          }
        }
      """)
    )

  test("update existence - can't delete a non-empty group") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid, Some(g1))
      _   <- expectOdbError(
              user = pi,
              query = s"""
                mutation {
                  updateGroups(input: {
                    SET: {
                      existence: DELETED
                    },
                    WHERE: {
                      id: { IN: [${g1.asJson}] }
                    }
                  }) {
                    groups {
                      id
                    }
                  }
                }
              """,
              expected = {
                case OdbError.InconsistentGroupError(Some("Deleted group contains non-deleted element(s).")) => ()
              }
            )
    yield ()
  }

  test("update existence - can delete an empty group") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      _   <- updateGroupExistenceAs(pi, g1, Existence.Deleted)
    yield ()
  }

  test("update existence - deleted group shouldn't be visible at top level") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      _   <- updateGroupExistenceAs(pi, g1, Existence.Deleted)
      _   <- assertIO(groupElementsAs(pi, pid, None), List(Left(g2)))
    yield ()
  }

  test("update existence - deleted group should be visible at top level, if you ask") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      _   <- updateGroupExistenceAs(pi, g1, Existence.Deleted)
      _   <- assertIO(groupElementsAs(pi, pid, None, includeDeleted = true), List(Left(g1), Left(g2)))
    yield ()
  }

  test("update existence - deleted group shouldn't be visible at nested level") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid, Some(g1))
      _   <- updateGroupExistenceAs(pi, g2, Existence.Deleted)
      _   <- assertIO(groupElementsAs(pi, pid, Some(g1)), Nil)
    yield ()
  }

  test("update existence - deleted group should be visible at nested level, if you ask") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid, Some(g1))
      _   <- updateGroupExistenceAs(pi, g2, Existence.Deleted)
      _   <- assertIO(groupElementsAs(pi, pid, Some(g1), includeDeleted = true), List(Left(g2)))
    yield ()
  }

  test("update existence - can undelete a deleted group") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      _   <- updateGroupExistenceAs(pi, g1, Existence.Deleted)
      _   <- updateGroupExistenceAs(pi, g1, Existence.Present)
    yield ()
  }

  def updateGroupName(user: User, gid: Group.Id, name: String): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              name: $name
            },
            WHERE: {
              id: { IN: [${gid.asJson}] }
            }
          }) {
            groups {
              id
              name
            }
          }
        }
      """,
      expected = Right(json"""
        {
          "updateGroups": {
            "groups": [
              {
                "id": $gid,
                "name": $name
              }
            ]
          }
        }
      """)
    )

  def updateGroupNameNoOp(user: User, gid: Group.Id, name: String): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              name: $name
            },
            WHERE: {
              id: { IN: [${gid.asJson}] }
            }
          }) {
            groups {
              id
              name
            }
          }
        }
      """,
      expected = Right(json"""
        {
          "updateGroups": {
            "groups": [
            ]
          }
        }
      """)
    )

  test("update name") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      _   <- updateGroupName(pi, g1, "NewName")
    yield ()
  }

  test("update system group's name is a no op") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      _   <- updateGroupSystem(g1, true)
      _   <- updateGroupNameNoOp(pi, g1, "NewName")
    yield ()
  }

  test("update system group's name is allowed for system") {
    for
      pid <- createProgramAs(service)
      g1  <- createGroupAs(service, pid)
      _   <- updateGroupSystem(g1, true)
      _   <- updateGroupName(service, g1, "NewName")
    yield ()
  }

  test("can create group with any value for minimumInterval and no maximumInterval") {
    for 
      pid <- createProgramAs(pi)
      _   <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromMinutes(5))
    yield ()
  }

  test("can create group with any value for maximumInterval and no minimumInterval") {
    for 
      pid <- createProgramAs(pi)
      _   <- createGroupAs(pi, pid, maximumInterval = TimeSpan.fromMinutes(5))
    yield ()
  }

  test("can create group with minimumInterval < maximumInterval") {
    for 
      pid <- createProgramAs(pi)
      _   <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromMinutes(5), maximumInterval = TimeSpan.fromMinutes(6))
    yield ()
  }

  test("can create group with minimumInterval == maximumInterval") {
    for 
      pid <- createProgramAs(pi)
      _   <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromMinutes(5), maximumInterval = TimeSpan.fromMinutes(5))
    yield ()
  }

  test("cannot create group with minimumInterval > maximumInterval") {
    // This error is caught by the Input
    def createGroupError(pid: Program.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            createGroup(
              input: {
                programId: "$pid"
                SET: {
                  minimumInterval: { seconds: 5 }
                  maximumInterval: { seconds: 4 }
                }
              }
            ) {
              group { id }
            }
          }
        """,
        expected = List(inputIntervalError).asLeft
      )
    
    for {
      pid <- createProgramAs(pi)
      _   <- createGroupError(pid)
    } yield ()
  }

  def updateGroupIntervals(
    user: User,
    gid: Group.Id,
    minimumInterval: Option[Option[TimeSpan]],
    maximumInterval: Option[Option[TimeSpan]],
    error: Option[String]
  ): IO[Unit] = 
    extension (oots: Option[Option[TimeSpan]])
      def toSetter(prefix: String): String =
        oots.foldMap(ots =>
          s"${prefix}imumInterval: ${ots.fold("null")(ts => s"{ microseconds: \"${ts.toMicroseconds.asJson.noSpaces}\" }")}"
          )

    val expectation: Either[List[String], Json] = error.fold(
      Right(json"""
              {
                "updateGroups": {
                  "groups": [
                    {
                      "id": $gid
                    }
                  ]
                }
              }
            """)
    )(s => Left(List(s)))

    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              ${minimumInterval.toSetter("min")}
              ${maximumInterval.toSetter("max")}
            },
            WHERE: {
              id: { EQ: ${gid.asJson} }
            }
          }) {
            groups {
              id
            }
          }
        }
      """,
      expected = expectation
    )

  test("Can update minimumInterval if maximumInterval is not set") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromSeconds(5))
      _   <- updateGroupIntervals(
        pi,
        gid,
        minimumInterval = TimeSpan.fromHours(3).some,
        maximumInterval = None,
        None)
    } yield ()
  }

  test("Can update maximumInterval if minimumInterval is not set") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, maximumInterval = TimeSpan.fromSeconds(5))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = None,
        maximumInterval = TimeSpan.fromHours(3).some,
        error = None)
    } yield ()
  }

  test("Can update maximumInterval to same as minimumInterval") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromHours(23))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = None,
        maximumInterval = TimeSpan.fromHours(23).some,
        error = None)
    } yield ()
  }

  test("Can update minimumInterval to same as maximumInterval") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, maximumInterval = TimeSpan.fromMinutes(59))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromMinutes(59).some,
        maximumInterval = None,
        error = None)
    } yield ()
  }

  test("Can update maximumInterval to greater than minimumInterval") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromHours(23))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = None,
        maximumInterval = TimeSpan.fromHours(24).some,
        error = None)
    } yield ()
  }

  test("Can update minimumInterval to less than maximumInterval") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromHours(23))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromHours(22).some,
        maximumInterval = None,
        error = None)
    } yield ()
  }

  test("Cannot update maximumInterval greater than minimumInterval") {
    // This error is caught by the database
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromHours(3))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = None,
        maximumInterval = TimeSpan.fromHours(2).some,
        error = odbIntervalError.some)
    } yield ()
  }

  test("Cannot update minimumInterval greater than maximumInterval") {
    // This error is caught by the database
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, maximumInterval = TimeSpan.fromHours(3))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromHours(5).some,
        maximumInterval = None,
        error = odbIntervalError.some)
    } yield ()
  }

  test("Can update both minimumInterval and maximumInterval to same") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, maximumInterval = TimeSpan.fromHours(3))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromMinutes(5).some,
        maximumInterval = TimeSpan.fromMinutes(5).some,
        error = None)
    } yield ()
  }

  test("Can update both minimumInterval and maximumInterval to valid values") {
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid, minimumInterval = TimeSpan.fromHours(3))
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromHours(10).some,
        maximumInterval = TimeSpan.fromHours(12).some,
        error = None)
    } yield ()
  }

  test("Cannot update both minimumInterval and maximumInterval to invalid values") {
    // This error is caught by the Input
    for {
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- updateGroupIntervals(pi,
        gid,
        minimumInterval = TimeSpan.fromHours(10).some,
        maximumInterval = TimeSpan.fromHours(9).some,
        error = inputIntervalError.some)
    } yield ()
  }
}
