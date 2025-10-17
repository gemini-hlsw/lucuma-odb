// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.EditType

import scala.concurrent.duration.*

class targetEdit extends OdbSuite {

  val pi = TestUsers.Standard.pi(11, 110)
  val service  = TestUsers.service(3)

  override def validUsers = List(pi, service)

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
          targetId
          value {
            name
          }
        }
      }
    """
  }

  def targetEdit(
    editType: EditType,
    name: String,
    tid: Target.Id
  ): Json =
    Json.obj(
      "targetEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "targetId" -> Json.fromString(tid.show),
        "value"    -> Json.obj(
          "name" -> Json.fromString(name)
        )
      )
    )

  def created(name: String, tid: Target.Id): Json =
    targetEdit(EditType.Created, name, tid)

  def updated(name: String, tid: Target.Id): Json =
    targetEdit(EditType.Updated, name, tid)

  def removeBlindOffset(user: User, oid: Observation.Id): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              targetEnvironment: {
                blindOffsetTarget: null
              }
            }
            WHERE: {
              id: { EQ: ${oid.asJson} }
            }
          }) {
            observations {
              targetEnvironment {
                blindOffsetTarget {
                  id
                }
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              {
                "targetEnvironment": {
                  "blindOffsetTarget": null
                }
              }
            ]
          }
        }
      """.asRight
    )

  test("trigger for a new target in any program") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      subscriptionExpectF(
        user      = pi,
        query     = nameSubscription(None, None),
        mutations =
          Right(
            createProgramAs(pi).flatMap(createTarget(pi, _, "target 1")
              .flatTap(i => ref.update(i :: _))) >>
            createProgramAs(pi).flatMap(createTarget(pi, _, "target 2")
              .flatTap(i => ref.update(i :: _)))
          ),
        expectedF = ref.get.map(l => List(created("target 1", l(1)), created("target 2", l(0))))
      )
    }
  }

  test("trigger for an updated target in any program") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      subscriptionExpectF(
        user      = pi,
        query     = nameSubscription(None, None),
        mutations =
          Right(
            for {
              pid <- createProgramAs(pi)
              tid <- createTarget(pi, pid, "old name").flatTap(i => ref.set(List(i)))
              _   <- updateTarget(pi, tid, "new name")
            } yield ()
          ),
        expectedF = ref.get.map(l => List(created("old name", l(0)), updated("new name", l(0))))
      )
    }
  }

  test("trigger for a new target in [only] a specific program") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      createProgramAs(pi).flatMap { pid =>
        subscriptionExpectF(
          user      = pi,
          query     = nameSubscription(Some(pid), None),
          mutations =
            Right(
              createTarget(pi, pid, "should see this").flatTap(i => ref.set(List(i))) >>
              createProgramAs(pi).flatMap(createTarget(pi, _, "should not see this"))
            ),
          expectedF = ref.get.map(l => List(created("should see this", l(0))))
        )
      }
    }
  }

  test("trigger for an updated target in [only] a specific program") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      createProgramAs(pi).flatMap { pid =>
        subscriptionExpectF(
          user      = pi,
          query     = nameSubscription(Some(pid), None),
          mutations =
            Right(
              createTarget(pi, pid, "should see this")
                .flatTap(i => ref.update(i :: _))
                .flatMap(updateTarget(pi, _, "and this")) >>
              createProgramAs(pi).flatMap(
                createTarget(pi, _, "should not see this")
                  .flatTap(i => ref.update(i :: _))
                  .flatMap(updateTarget(pi, _, "or this")))
            ),
          expectedF = ref.get.map(l => List(created("should see this", l(1)), updated("and this", l(1))))
        )
      }
    }
  }

  test("trigger for [only] a specific updated target") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      createProgramAs(pi).flatMap { pid =>
        createTarget(pi, pid, "old name")
          .flatTap(i => ref.set(List(i)))
          .flatMap { tid =>
            subscriptionExpectF(
              user      = pi,
              query     = nameSubscription(None, Some(tid)),
              mutations =
                Right(
                  updateTarget(pi, tid, "new name") >>
                  createTarget(pi, pid, "should not see this").flatMap(updateTarget(pi, _, "or this"))
                ),
              expectedF = ref.get.map(l => List(updated("new name", l(0))))
            )
        }
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
          }
        }
      """,
      mutations =
        Right(
          createProgramAs(pi).flatMap(createTarget(pi, _, "t")).replicateA(2)
        ),
      expected = List.fill(2)(json"""{"targetEdit":{"editType":"CREATED"}}""")
    )
  }

  test("event for calibration target deletion") {
    Ref.of[IO, List[Target.Id]](List.empty[Target.Id]).flatMap { ref =>
      subscriptionExpectF(
        user      = pi,
        query     = s"""
          subscription {
            targetEdit {
              editType
              targetId
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            createProgramAs(pi)
              .flatTap(pid =>
              (createTargetViaServiceAs(pi, pid, TargetDisposition.Calibration, CalibrationRole.Twilight.some) <* pause)
                  .flatTap(i => ref.set(List(i)))
              )
              .flatMap(pid => withServices(service) {services =>
                services.session.transaction.use { xa =>
                  services.targetService.deleteOrphanCalibrationTargets(pid)(using xa)
                }.onError { case r => IO(r.printStackTrace()) }
              })
          ),
        expectedF = ref.get.map(i =>
              List(
                json"""{
                    "targetEdit": {
                      "editType":"CREATED",
                      "targetId":${i.head.show},
                      "value": {
                        "id":${i.head.show}
                      }
                    }
                  }""",
                json"""{
                    "targetEdit": {
                      "editType":"HARD_DELETE",
                      "targetId":${i.head.show},
                      "value": null
                    }
                  }"""
             )
            )
      )
    }
  }

  test("event for blind offset target deletion") {
    Ref.of[IO, List[(Observation.Id, Target.Id)]](List.empty[(Observation.Id, Target.Id)]).flatMap { ref =>
      subscriptionExpectF(
        user      = pi,
        query     = s"""
          subscription {
            targetEdit {
              editType
              targetId
              value {
                id
              }
            }
          }
        """,
        mutations =
          Right(
            createProgramAs(pi)
              .flatMap(pid =>
                (createObservationWithBlindOffsetAs(pi, pid, "Blinding") <* pause)
                  .flatTap(tup => ref.set(List(tup)))
              .flatMap((oid, _) => removeBlindOffset(pi, oid))
            )
          ),
        expectedF = ref.get.map(l =>
          val tid   = l.head._2
          List(
            json"""{
                "targetEdit": {
                  "editType": "CREATED",
                  "targetId": ${tid.asJson},
                  "value": {
                    "id":${tid.asJson}
                  }
                }
              }""",
            json"""{
                "targetEdit": {
                  "editType":"HARD_DELETE",
                  "targetId":${tid.asJson},
                  "value": null
                }
              }"""
          )
        )
      )
    }
  }
}
