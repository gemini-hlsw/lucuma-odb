// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.show.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.EditType

import scala.concurrent.duration.*

class targetEdit extends OdbSuite {

  val pi = TestUsers.Standard.pi(11, 110)

  override def validUsers = List(pi)

  val pause = IO.sleep(300.milli)

  def createTarget(user: User, pid: Program.Id, name: String) =
    createTargetAs(user, pid, name) <* pause

  def updateTarget(user: User, tid: Target.Id, name: String) =
    expect(
      user = user,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: { name: "$name" }
            WHERE: {
              id: { EQ: "$tid"}
            }
          }) {
            targets {
              id
              name
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "updateTargets" : {
              "targets" : [
                {
                  "id" : $tid,
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
    oid: Option[Target.Id]
  ): String = {
    val args: String =
      (pid, oid) match {
        case (Some(p), Some(o)) => s"""(input: { programId: "${p.show}", targetId: "${o.show}" } )"""
        case (Some(p), None   ) => s"""(input: { programId: "${p.show}" } )"""
        case (None,    Some(o)) => s"""(input: { targetId: "${o.show}" } )"""
        case (None,    None   ) => ""
      }

    s"""
      subscription {
        targetEdit$args {
          editType
          value {
            name
          }
        }
      }
    """
  }

  def targetEdit(
    editType: EditType,
    name: String
  ): Json =
    Json.obj(
      "targetEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "value"    -> Json.obj(
          "name" -> Json.fromString(name)
        )
      )
    )

  def created(name: String): Json =
    targetEdit(EditType.Created, name)

  def updated(name: String): Json =
    targetEdit(EditType.Updated, name)

  test("trigger for a new target in any program") {
    subscriptionExpect(
      user      = pi,
      query     = nameSubscription(None, None),
      mutations =
        Right(
          createProgramAs(pi).flatMap(createTarget(pi, _, "target 1")) >>
          createProgramAs(pi).flatMap(createTarget(pi, _, "target 2"))
        ),
      expected = List(created("target 1"), created("target 2"))
    )
  }

  test("trigger for an updated target in any program") {
    subscriptionExpect(
      user      = pi,
      query     = nameSubscription(None, None),
      mutations =
        Right(
          for {
            pid <- createProgramAs(pi)
            tid <- createTarget(pi, pid, "old name")
            _   <- updateTarget(pi, tid, "new name")
          } yield ()
        ),
      expected = List(created("old name"), updated("new name"))
    )
  }

  test("trigger for a new target in [only] a specific program") {
    createProgramAs(pi).flatMap { pid =>
      subscriptionExpect(
        user      = pi,
        query     = nameSubscription(Some(pid), None),
        mutations =
          Right(
            createTarget(pi, pid, "should see this") >>
            createProgramAs(pi).flatMap(createTarget(pi, _, "should not see this"))
          ),
        expected = List(created("should see this"))
      )
    }
  }

  test("trigger for an updated target in [only] a specific program") {
    createProgramAs(pi).flatMap { pid =>
      subscriptionExpect(
        user      = pi,
        query     = nameSubscription(Some(pid), None),
        mutations =
          Right(
            createTarget(pi, pid, "should see this").flatMap(updateTarget(pi, _, "and this")) >>
            createProgramAs(pi).flatMap(createTarget(pi, _, "should not see this").flatMap(updateTarget(pi, _, "or this")))
          ),
        expected = List(created("should see this"), updated("and this"))
      )
    }
  }

  test("trigger for [only] a specific updated target") {
    createProgramAs(pi).flatMap { pid =>
      createTarget(pi, pid, "old name").flatMap { tid =>
        subscriptionExpect(
          user      = pi,
          query     = nameSubscription(None, Some(tid)),
          mutations =
            Right(
              updateTarget(pi, tid, "new name") >>
              createTarget(pi, pid, "should not see this").flatMap(updateTarget(pi, _, "or this"))
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
          targetEdit {
            editType
            id
          }
        }
      """,
      mutations =
        Right(
          createProgramAs(pi).flatMap(createTarget(pi, _, "t")).replicateA(2)
        ),
      expected = List.fill(2)(json"""{"targetEdit":{"editType":"CREATED","id":0}}""")
    )
  }


}
