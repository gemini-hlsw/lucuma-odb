// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.show.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType

import scala.concurrent.duration._

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
    }

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
          "name" -> Json.fromString(name)
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

}
