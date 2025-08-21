// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.show.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType

import scala.concurrent.duration.*

class groupEdit extends OdbSuite {

  val pi = TestUsers.Standard.pi(11, 110)

  override def validUsers = List(pi)

  val pause = IO.sleep(300.milli)

  def createGroup(user: User, pid: Program.Id, name: String): IO[Group.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          createGroup(
            input: {
              programId: ${pid.asJson}
              SET: {
                name: ${name.asJson}
              }
            }
          ) {
            group {
              id
            }
          }
        }
        """
    ).map { json =>
      json.hcursor.downFields("createGroup", "group", "id").require[Group.Id]
    } <* pause

  def updateGroup(user: User, gid: Group.Id, name: String) =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: { name: "$name" }
            WHERE: {
              id: { EQ: "$gid"}
            }
          }) {
            groups {
              id
              name
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "updateGroups" : {
              "groups" : [
                {
                  "id" : $gid,
                  "name" : $name
                }
              ]
            }
          }
        """
      )
    ) >> pause

  def nameSubscription(
    pid: Option[Program.Id],
    oid: Option[Group.Id]
  ): String = {
    val args: String =
      (pid, oid) match {
        case (Some(p), Some(o)) => s"""(input: { programId: "${p.show}", groupId: "${o.show}" } )"""
        case (Some(p), None   ) => s"""(input: { programId: "${p.show}" } )"""
        case (None,    Some(o)) => s"""(input: { groupId: "${o.show}" } )"""
        case (None,    None   ) => ""
      }

    s"""
      subscription {
        groupEdit$args {
          editType
          value {
            name
            parentId
            parentIndex
          }
        }
      }
    """
  }

  def groupEdit(
    editType: EditType,
    name: String
  ): Json =
    Json.obj(
      "groupEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "value"    -> Json.obj(
          "name"        -> Json.fromString(name),
          "parentId"    -> Json.Null,
          "parentIndex" -> Json.fromInt(0),
        )
      )
    )

  def created(name: String): Json =
    groupEdit(EditType.Created, name)

  def updated(name: String): Json =
    groupEdit(EditType.Updated, name)

  test("trigger for a new group in any program") {
    subscriptionExpect(
      user      = pi,
      query     = nameSubscription(None, None),
      mutations =
        Right(
          createProgramAs(pi).flatMap(createGroup(pi, _, "group 1")) >>
          createProgramAs(pi).flatMap(createGroup(pi, _, "group 2"))
        ),
      expected = List(created("group 1"), created("group 2"))
    )
  }

  test("trigger for an updated group in any program") {
    subscriptionExpect(
      user      = pi,
      query     = nameSubscription(None, None),
      mutations =
        Right(
          for {
            pid <- createProgramAs(pi)
            gid <- createGroup(pi, pid, "old name")
            _   <- updateGroup(pi, gid, "new name")
          } yield ()
        ),
      expected = List(created("old name"), updated("new name"))
    )
  }

  test("trigger for a new group in [only] a specific program") {
    createProgramAs(pi).flatMap { pid =>
      subscriptionExpect(
        user      = pi,
        query     = nameSubscription(Some(pid), None),
        mutations =
          Right(
            createGroup(pi, pid, "should see this") >>
            createProgramAs(pi).flatMap(createGroup(pi, _, "should not see this"))
          ),
        expected = List(created("should see this"))
      )
    }
  }

  test("trigger for an updated group in [only] a specific program") {
    createProgramAs(pi).flatMap { pid =>
      subscriptionExpect(
        user      = pi,
        query     = nameSubscription(Some(pid), None),
        mutations =
          Right(
            createGroup(pi, pid, "should see this").flatMap(updateGroup(pi, _, "and this")) >>
            createProgramAs(pi).flatMap(createGroup(pi, _, "should not see this").flatMap(updateGroup(pi, _, "or this")))
          ),
        expected = List(created("should see this"), updated("and this"))
      )
    }
  }

  test("trigger for [only] a specific updated group") {
    createProgramAs(pi).flatMap { pid =>
      createGroup(pi, pid, "old name").flatMap { gid =>
        subscriptionExpect(
          user      = pi,
          query     = nameSubscription(None, Some(gid)),
          mutations =
            Right(
              updateGroup(pi, gid, "new name") >>
              createGroup(pi, pid, "should not see this").flatMap(updateGroup(pi, _, "or this"))
            ),
          expected = List(updated("new name"))
        )
      }
    }
  }

  test("work even if no database fields are selected") {
     subscriptionExpect(
       user      = pi,
       query     = s"""
         subscription {
           groupEdit {
             editType
           }
         }
       """,
       mutations =
         Right(
           createProgramAs(pi).flatMap(createGroupAs(pi, _)).replicateA(2)
         ),
       expected = List.fill(2)(json"""{"groupEdit":{"editType":"CREATED"}}""")
     )
   }

  test("include correct parent information") {
    Deferred[IO, (Group.Id, Group.Id, Group.Id)].flatMap { gids =>
      subscriptionExpectF(
        user      = pi,
        query     = s"""
          subscription {
            groupEdit {
              editType
              value {
                id
                parentId
                parentIndex
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              g1  <- createGroupAs(pi, pid)
              g2  <- createGroupAs(pi, pid, Some(g1))
              g3  <- createGroupAs(pi, pid, Some(g1), Some(NonNegShort.unsafeFrom(0)))
              _   <- gids.complete((g1, g2, g3))
            } yield ()
          ),
        expectedF =
          gids.get.map { (g1, g2, g3) =>
            List(
              // first group created
              json"""{
                "groupEdit" : {
                 "editType" : "CREATED",
                  "value" : {
                    "id" : $g1,
                    "parentId" : null,
                    "parentIndex" : 0
                  }
                }
              }""",
              // second group created
              json"""{
                "groupEdit" : {
                  "editType" : "CREATED",
                  "value" : {
                    "id" : $g2,
                    "parentId" : $g1,
                    "parentIndex" : 0
                  }
                }
              }""",
              // second group moved out of the way
              json"""{
                "groupEdit" : {
                  "editType" : "UPDATED",
                  "value" : {
                    "id" : $g2,
                    "parentId" : $g1,
                    "parentIndex" : 1
                  }
                }
              }""",
              // third group created
              json"""{
                "groupEdit" : {
                  "editType" : "CREATED",
                  "value" : {
                    "id" : $g3,
                    "parentId" : $g1,
                    "parentIndex" : 0
                  }
                }
              }"""        
            )
          }
      )
    }
  }

  test("don't trigger on top-level observation creation") {
    Deferred[IO, Program.Id].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              editType
              program {
                id
              }
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              _   <- d.complete(pid)
              oid <- createObservationAs(pi, pid)
              _   <- IO.sleep(300.milli)
            } yield ()
          ),
        expectedF =
          d.get.map { _ =>
            List.empty[Json]
          }
      )
    }
  }

  test("trigger on in-group observation creation") {
    Deferred[IO, (Program.Id, Group.Id)].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              editType
              program {
                id
              }
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              gid <- createGroupAs(pi, pid, None, None, None)
              oid <- createObservationInGroupAs(pi, pid, Some(gid), None)
              _   <- d.complete((pid, gid))
              _   <- IO.sleep(300.milli)
            } yield ()
          ),
        expectedF =
          d.get.map { (pid, gid) =>
            List(
              json"""{
                "groupEdit" : {
                  "editType" : "CREATED",
                  "program" : {
                    "id" : $pid
                  },
                  "value" : {
                    "id" : $gid
                  }
                }
              }""",
              json"""{
                "groupEdit" : {
                  "editType" : "UPDATED",
                  "program" : {
                    "id" : $pid
                  },
                  "value" : {
                    "id" : $gid
                  }
                }
              }"""
            )
          }
      )
    }
  }

  test("trigger on observation move from root group") {
    Deferred[IO, (Program.Id, Group.Id)].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              editType
              program {
                id
              }
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              gid <- createGroupAs(pi, pid, None, None, None)
              oid <- createObservationAs(pi, pid)
              _   <- moveObservationAs(pi, oid, Some(gid))
              _   <- d.complete((pid, gid))
            } yield ()
          ),
        expectedF =
          d.get.map { (pid, gid) =>
            List(
              // group created
              json"""{
                "groupEdit" : {
                  "editType" : "CREATED",
                  "program" : {
                    "id" : $pid
                  },
                  "value" : {
                    "id" : $gid
                  }
                }
              }""",
              // observation inserted into group
              json"""{
                "groupEdit" : {
                  "editType" : "UPDATED",
                  "program" : {
                    "id" : $pid
                  },
                  "value" : {
                    "id" : $gid
                  }
                }
              }"""
            )
          }
      )
    }
  }

  test("trigger on observation move from root group (limited selection)") {
    Deferred[IO, (Program.Id, Group.Id)].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              gid <- createGroupAs(pi, pid, None, None, None)
              oid <- createObservationAs(pi, pid)
              _   <- moveObservationAs(pi, oid, Some(gid))
              _   <- d.complete((pid, gid))
            } yield ()
          ),
        expectedF =
          d.get.map { (_, gid) =>
            List(
              // group created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid
                  }
                }
              }""",
              // observation inserted into group
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid
                  }
                }
              }"""
            )
          }
      )
    }
  }

  test("trigger on observation move between groups") {
    Deferred[IO, (Program.Id, Group.Id, Group.Id)].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid  <- createProgramAs(pi)
              gid1 <- createGroupAs(pi, pid, None, None, None)
              gid2 <- createGroupAs(pi, pid, None, None, None)
              oid  <- createObservationAs(pi, pid)
              _    <- moveObservationAs(pi, oid, Some(gid1))
              _    <- moveObservationAs(pi, oid, Some(gid2))
              _    <- d.complete((pid, gid1, gid2))
            } yield ()
          ),
        expectedF =
          d.get.map { (_, gid1, gid2) =>
            List(
              // group 1 created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid1
                  }
                }
              }""",
              // group 2 created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid2
                  }
                }
              }""",
              // observation inserted into group 1
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid1
                  }
                }
              }""",
              // observation inserted into group 2 - original group
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid1
                  }
                }
              }""",
              // observation moved into group 2 - new group
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid2
                  }
                }
              }"""
            )
          }
      )
    }
  }

  test("trigger on group move between groups, including event for old parent") {
    Deferred[IO, (Program.Id, Group.Id, Group.Id, Group.Id)].flatMap { d =>
      subscriptionExpectF(
        user = pi,
        query     = s"""
          subscription {
            groupEdit {
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            for {
              pid  <- createProgramAs(pi)
              gid1 <- createGroupAs(pi, pid, None, None, None)
              gid2 <- createGroupAs(pi, pid, None, None, None)
              gid3 <- createGroupAs(pi, pid, None, None, None)
              _    <- moveGroupAs(pi, gid3, Some(gid2))
              _    <- moveGroupAs(pi, gid3, Some(gid1))
              _    <- d.complete((pid, gid1, gid2, gid3))
            } yield ()
          ),
        expectedF =
          d.get.map { (_, gid1, gid2, gid3) =>
            List(
              // group 1 created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid1
                  }
                }
              }""",
              // group 2 created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid2
                  }
                }
              }""",
              // group 3 created
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid3
                  }
                }
              }""",
              // group 3 moved into group 2
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid3
                  }
                }
              }""",
              // group 2 moved into group 1
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid3
                  }
                }
              }""",
              // the parent of the above move
              json"""{
                "groupEdit" : {
                  "value" : {
                    "id" : $gid2
                  }
                }
              }"""
            )
          }
      )
    }
  }

}
