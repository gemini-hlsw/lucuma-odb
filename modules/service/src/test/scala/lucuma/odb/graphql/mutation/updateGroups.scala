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
      assertEquals(es.toSet, Set(Left(o2), Left(o1), Left(o3)))
      assertEquals(es.drop(1).toSet, Set(Left(o2), Left(o1)))
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
      assertEquals(es.toSet, Set(Left(o2), Left(o1), Left(o3)))
      assertEquals(es.dropRight(1).toSet, Set(Left(o2), Left(o1)))
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
      assertEquals(es.toSet, Set(Left(o2), Left(o1), Left(o3), Left(o4)))
      assertEquals(es.drop(1).dropRight(1).toSet, Set(Left(o2), Left(o1)))
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
      assertEquals(es.drop(2).toSet, Set(Left(o2), Left(o3)))
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
      assertEquals(es.take(2).toSet, Set(Left(o2), Left(o3)))
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
      assertEquals(es.drop(1).take(2).toSet, Set(Left(o2), Left(o3)))
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
      assertEquals(e2.toSet, Set(Left(o2), Left(o3)))
    }  
  }


}
