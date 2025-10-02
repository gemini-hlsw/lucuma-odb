// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import munit.Location

class blindOffsetTarget extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2)

  private val NoTargetTitle = "Untargeted"

  private def blindOffsetInput(name: String, ra: String, dec: String): String =
    s"""
      name: "$name"
      sidereal: {
        ra: { degrees: "$ra" }
        dec: { degrees: "$dec" }
        epoch: "J2000.000"
      }
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: {
              stellarLibrary: B5_III
            }
            brightnesses: []
          }
        }
      }
    """

  private val targetEnvironmentFields =
    """
      targetEnvironment {
        blindOffsetTarget {
          name
        }
      }
    """

  private val blindOffsetFields =
    s"""
      {
        title
        useBlindOffset
        $targetEnvironmentFields
      }
    """

  private val observationsFields =
    s"""
      observations $blindOffsetFields
    """

  private def blindOffsetTargetInput(createInput: String): String =
    s"""
      blindOffsetTarget: {
        $createInput
      }
    """

  private def expectedInnerResults(obsTitle: String, useBlindOffset: Boolean, blindOffsetName: Option[String]) =
    val blindOffset: Json =
      blindOffsetName.fold(Json.Null) { name =>
        Json.obj(
          "name" -> name.asJson
        )
      }
    json"""
      {
        "title": ${obsTitle.asJson},
        "useBlindOffset": $useBlindOffset,
        "targetEnvironment": {
          "blindOffsetTarget": $blindOffset
        }
      }
    """

  private def expectedResults(
    outterObj: String,
    innerObj: String,
    obsTitle: String,
    useBlindOffset: Boolean,
    blindOffsetName: Option[String]
  ) =
    Json.obj(outterObj -> Json.obj(innerObj -> expectedInnerResults(obsTitle, useBlindOffset, blindOffsetName))).asRight

  private def expectedListResults(
    outterObj: String,
    innerObj: String,
    obsTitle: String,
    useBlindOffset: Boolean,
    blindOffsetName: Option[String]
  ) =
    Json.obj(outterObj -> Json.obj(innerObj -> Json.arr(expectedInnerResults(obsTitle, useBlindOffset, blindOffsetName)))).asRight

  private def queryBlindOffsetId(user: User, oid: Observation.Id): IO[Option[Target.Id]] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: ${oid.asJson}) {
            targetEnvironment {
              blindOffsetTarget {
                id
              }
            }
          }
        }
      """
    ).map:
      _.hcursor
        .downField("observation")
        .downField("targetEnvironment")
        .downField("blindOffsetTarget")
        .downField("id").as[Target.Id]
        .toOption
  
  private def getBlindOffsetId(user: User, oid: Observation.Id)(using Location): IO[Target.Id] = 
    queryBlindOffsetId(user, oid).map:
      _.getOrElse(fail("Expected blind offset target"))

  private def expectNoBlindOffset(user: User, oid: Observation.Id)(using Location): IO[Unit] =
    queryBlindOffsetId(user, oid).map:
      _.fold(())(_ => fail("Expected no blind offset target"))

  private def expectBlindOffsetId(user: User, oid: Observation.Id, expected: Target.Id)(using Location): IO[Unit] =
    assertIO(getBlindOffsetId(user, oid), expected)

  private def expectAsterismTargetsAs(user: User, oid: Observation.Id, expected: Target.Id*)(using Location): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            targetEnvironment {
              asterism {
                id
              }
            }
          }
        }
      """,
      expected =
        json"""
        {
          "observation": {
            "targetEnvironment": {
              "asterism": 
                ${expected.map { tid =>
                  json"""
                    {
                      "id": ${tid.asJson}
                    }
                  """
                }}
            }
          }
        }
      """.asRight
    )

  test("create observation with blind offset target"):
    for {
      pid <- createProgramAs(pi)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
            }) {
              observation $blindOffsetFields
            }
          }
        """,
        expected = expectedResults("createObservation", "observation", NoTargetTitle, true, "Blind Offset Star".some)
      )
    } yield () 

  test("create observation with blind offset target and non-blind offset target"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                  asterism: [${tid.asJson}]
                }
              }
            }) {
              observation $blindOffsetFields
            }
          }
        """,
        expected = expectedResults("createObservation", "observation", "Regular Target", true, "Blind Offset Star".some)
      )
    } yield () 

  test("create observation with no targets"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- expect(
        user = pi,
        query = s"""
          query {
            observation(observationId: ${oid.asJson}) $blindOffsetFields
          }
        """,
        expected = json"""
          {
            "observation": {
              "title": ${NoTargetTitle.asJson},
              "useBlindOffset": false,
              "targetEnvironment": {
                "blindOffsetTarget": null
              }
            }
          }
        """.asRight
      )
      _ <- expectAsterismTargetsAs(pi, oid) // blind offset targets are not in the asterism
      _ <- expectNoBlindOffset(pi, oid)
    } yield ()

  test("create observation with non-blind offset target"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          query {
            observation(observationId: ${oid.asJson}) $blindOffsetFields
          }
        """,
        expected = json"""
          {
            "observation": {
              "title": ${"Regular Target".asJson},
              "useBlindOffset": false,
              "targetEnvironment": {
                "blindOffsetTarget": null
              }
            }
          }
        """.asRight
      )
      _ <- expectNoBlindOffset(pi, oid)
    } yield ()

  test("blind offsets are not in the observation asterism"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Regular Target 1")
      tid2 <- createTargetAs(pi, pid, "Regular Target 2")
      (oid, btid)  <- createObservationWithBlindOffsetAs(pi, pid, "Blind Offset", tid1, tid2)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- expectAsterismTargetsAs(pi, oid, tid1, tid2) // blind offset targets are not in the asterism
    } yield ()

  test("update observation to add blind offset target"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("New Blind Offset Target", "98.765", "-12.345"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "New Blind Offset Target".some)
      )
      _ <- getBlindOffsetId(pi, oid)
      _ <- expectAsterismTargetsAs(pi, oid) // blind offset targets are not in the asterism
    } yield ()

  test("update observation to replace blind offset target"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Original Target", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "Original Target".some)
      )
      tid <- getBlindOffsetId(pi, oid)
      _   <- expectTargetExistenceAs(pi, tid, shouldBePresent = true)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Replacement Target", "99.999", "-88.888"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "Replacement Target".some)
      )
      _ <- expectTargetNotFoundAs(pi, tid)
      _ <- getBlindOffsetId(pi, oid)
    } yield ()

  test("update observation to remove blind offset target"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _   <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Target to Remove", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Target to Remove".some)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- expectAsterismTargetsAs(pi, oid, tid) 
      _    <- expect(
        user = pi,
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
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, none)
      )
      _   <- expectTargetNotFoundAs(pi, btid)
      _   <- expectAsterismTargetsAs(pi, oid, tid) 
      _   <- expectNoBlindOffset(pi, oid)
    } yield ()

  test("create observation with blind offset target and non-blind offset target, set useBlindOffset to false"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- expectAsterismTargetsAs(pi, oid, tid) 
      _    <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                useBlindOffset: false
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", false, none)
      )
      _    <- expectTargetNotFoundAs(pi, btid)
      _    <- expectAsterismTargetsAs(pi, oid, tid) 
      _    <- expectNoBlindOffset(pi, oid)
    } yield () 

  test("create observation with blind offset target and non-blind offset target, delete and restore"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- deleteObservation(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = false)
      _    <- restoreObservation(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
    } yield () 

  test("removing blind offset from an observation via the asterism service is an error"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- expectAsterismTargetsAs(pi, oid, tid)
      _    <- expect(
        user = pi,
        query = s"""
          mutation {
            updateAsterisms(input: {
              SET: { DELETE: [${btid.asJson}] }
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) {
              observations { id }
            }
          }
        """,
        expected = List("Blind offset targets cannot be removed from an asterism.").asLeft
      )
      _ <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _ <- expectBlindOffsetId(pi, oid, btid)
      _ <- expectAsterismTargetsAs(pi, oid, tid)
    } yield () 

  test("adding blind offset to an observation via the asterism service is an error"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "Regular Target")
      oid <- createObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """,
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some)
      )
      btid <- getBlindOffsetId(pi, oid)
      oid2 <- createObservationAs(pi, pid)
      _    <- expect(
        user = pi,
        query = s"""
          mutation {
            updateAsterisms(input: {
              SET: { ADD: [${btid.asJson}] }
              WHERE: { id: { EQ: ${oid2.asJson} } }
            }) {
              observations { id }
            }
          }
        """,
        expected = List("Blind offset targets cannot be added to an asterism.").asLeft
      )
      _ <- expectBlindOffsetId(pi, oid, btid)
      _ <- expectNoBlindOffset(pi, oid2)
    } yield () 

  // This does cause an exception to thrown via skunk, but I don't know how to catch it because it is
  // a deferred constraint. Since we don't allow creating non-science targets via the API, this should
  // never happen in practice.
  // test("creating blind offset without obs is error"):
  //   for
  //     pid <- createProgramAs(pi)
  //     tid <- createTargetViaServiceAs(pi, pid, TargetDisposition.BlindOffset, none)
  //   yield ()
