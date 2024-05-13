// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.User

class updateGroups extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

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

  def deleteGroupAs(user: User, gid: Group.Id): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              existence: DELETED
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
                "existence": "DELETED"
              }
            ]
          }
        }
      """)
    )

  test("update existence - can't delete a non-empty group".fail) {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      -   <- createGroupAs(pi, pid, Some(g1), None)
      _   <- deleteGroupAs(pi, g1)
    yield ()
  }

  test("update existence - can delete a non-empty group") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      _   <- deleteGroupAs(pi, g1)
    yield ()
  }

  test("update existence - deleted group shouldn't be visible at top level") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      _   <- deleteGroupAs(pi, g1)
      _   <- assertIO(groupElementsAs(pi, pid, None), List(Left(g2)))
    yield ()
  }
  
  test("update existence - deleted group should be visible at top level, if you ask") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid)
      _   <- deleteGroupAs(pi, g1)
      _   <- assertIO(groupElementsAs(pi, pid, None, includeDeleted = true), List(Left(g1), Left(g2)))
    yield ()
  }

  test("update existence - deleted group shouldn't be visible at nested level") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid, Some(g1))
      _   <- deleteGroupAs(pi, g2)
      _   <- assertIO(groupElementsAs(pi, pid, None), List(Left(g1)))
    yield ()
  }
  
  test("update existence - deleted group should be visible at nested level, if you ask") {
    for
      pid <- createProgramAs(pi)
      g1  <- createGroupAs(pi, pid)
      g2  <- createGroupAs(pi, pid, Some(g1))
      _   <- deleteGroupAs(pi, g2)
      _   <- assertIO(groupElementsAs(pi, pid, None), List(Left(g1), Left(g2)))
    yield ()
  }

}
