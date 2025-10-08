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
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.util.Codecs.*
import munit.Location
import skunk.SqlState
import skunk.syntax.all.*

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
        explicitBlindOffset
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

  private def expectedInnerResults(obsTitle: String, useBlindOffset: Boolean, blindOffsetName: Option[String], isExplicit: Boolean) =
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
          "blindOffsetTarget": $blindOffset,
          "explicitBlindOffset": $isExplicit
        }
      }
    """

  private def expectedResults(
    outterObj: String,
    innerObj: String,
    obsTitle: String,
    useBlindOffset: Boolean,
    blindOffsetName: Option[String],
    isExplicit: Boolean
  ) =
    Json.obj(outterObj -> Json.obj(innerObj -> expectedInnerResults(obsTitle, useBlindOffset, blindOffsetName, isExplicit))).asRight

  private def expectedListResults(
    outterObj: String,
    innerObj: String,
    obsTitle: String,
    useBlindOffset: Boolean,
    blindOffsetName: Option[String],
    isExplicit: Boolean
  ) =
    Json.obj(outterObj -> Json.obj(innerObj -> Json.arr(expectedInnerResults(obsTitle, useBlindOffset, blindOffsetName, isExplicit)))).asRight

  private def queryBlindOffsetFields(user: User, oid: Observation.Id): IO[(Boolean, Option[Target.Id], Option[String], Boolean)] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: ${oid.asJson}) {
            useBlindOffset
            targetEnvironment {
              blindOffsetTarget {
                id
                name
              }
              explicitBlindOffset
            }
          }
        }
      """
    ).map: json =>
      val obs = json.hcursor.downField("observation")
      val useBlindOffset = obs.downField("useBlindOffset").require[Boolean]
      val te = obs.downField("targetEnvironment")
      val btid = te.downField("blindOffsetTarget").downField("id").as[Target.Id].toOption
      val name = te.downField("blindOffsetTarget").downField("name").as[String].toOption
      val isExplicit = te.downField("explicitBlindOffset").require[Boolean]
      (useBlindOffset, btid, name, isExplicit)
  
  private def getBlindOffsetId(user: User, oid: Observation.Id)(using Location): IO[Target.Id] = 
    queryBlindOffsetFields(user, oid).map:
      _._2.getOrElse(fail("Expected blind offset target"))

  private def expectBlindOffsetFields(
    user: User,
    oid: Observation.Id,
    useBlindOffset: Boolean,
    btid: Option[Target.Id],
    name: Option[String],
    isExplicit: Boolean
  )(using Location): IO[Unit] = 
    assertIO(queryBlindOffsetFields(user, oid), (useBlindOffset, btid, name, isExplicit), "Unexpected blind offset fields")

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

  def cloneObservationWithTargetEnvironment(user: User, oid: Observation.Id, tenv: String): IO[Observation.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          cloneObservation(input: {
            observationId: "$oid"
            SET: {
              targetEnvironment: $tenv
            }
          }) {
            newObservation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id])

  // tests the database trigger
  private def insertIntoAstersimViaSqlAndExpectError(pid: Program.Id, oid: Observation.Id, blindtid: Target.Id)(using Location): IO[Unit] =
    val gotExpected = withSession:
      _.execute(
        sql"""
          INSERT INTO t_asterism_target
          VALUES ($program_id, $observation_id, $target_id)
        """.command
      )(pid, oid, blindtid)
      .map(_ => false)
      .recover:
        case SqlState.RaiseException(m)
          if m.getMessage.contains("An asterism can only contain science and calibration targets.") => true 
    assertIOBoolean(gotExpected)

  private def hardDeleteTarget(tid: Target.Id): IO[Unit] =
    withSession:
      _.execute(sql"""DELETE FROM t_target WHERE c_target_id = $target_id""".command)(tid).void

  test("create observation with explicit blind offset target"):
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
        expected = expectedResults("createObservation", "observation", NoTargetTitle, true, "Blind Offset Star".some, true)
      )
    } yield () 

  test("create observation with automatic blind offset target"):
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
                  explicitBlindOffset: false
                }
              }
            }) {
              observation $blindOffsetFields
            }
          }
        """,
        expected = expectedResults("createObservation", "observation", NoTargetTitle, true, "Blind Offset Star".some, false)
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
        expected = expectedResults("createObservation", "observation", "Regular Target", true, "Blind Offset Star".some, true)
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
                "blindOffsetTarget": null,
                "explicitBlindOffset": false
              }
            }
          }
        """.asRight
      )
      _ <- expectAsterismTargetsAs(pi, oid) // blind offset targets are not in the asterism
      _ <- expectBlindOffsetFields(pi, oid, false, none, none, false)
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
                "blindOffsetTarget": null,
                "explicitBlindOffset": false
              }
            }
          }
        """.asRight
      )
      _ <- expectBlindOffsetFields(pi, oid, false, none, none, false)
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

  test("update observation to add explicit blind offset target"):
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
                  explicitBlindOffset: true
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
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "New Blind Offset Target".some, true)
      )
      _ <- getBlindOffsetId(pi, oid)
      _ <- expectAsterismTargetsAs(pi, oid) // blind offset targets are not in the asterism
    } yield ()

  test("update observation to add automatic blind offset target"):
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
                  explicitBlindOffset: false
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
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "New Blind Offset Target".some, false)
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
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "Original Target".some, true)
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
        expected = expectedListResults("updateObservations", "observations", NoTargetTitle, true, "Replacement Target".some, true)
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Target to Remove".some, true)
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, none, false)
      )
      _   <- expectTargetNotFoundAs(pi, btid)
      _   <- expectAsterismTargetsAs(pi, oid, tid) 
      _   <- expectBlindOffsetFields(pi, oid, true, none, none, false) // useOffset should still be true
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some, true)
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", false, none, false)
      )
      _    <- expectTargetNotFoundAs(pi, btid)
      _    <- expectAsterismTargetsAs(pi, oid, tid) 
      _    <- expectBlindOffsetFields(pi, oid, false, none, none, false)
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some, true)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- deleteObservation(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = false)
      _    <- restoreObservation(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
    } yield () 

  test("removing blind offset from an observation via the asterism service does nothing"):
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some, true)
      )
      btid <- getBlindOffsetId(pi, oid)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- expectAsterismTargetsAs(pi, oid, tid)
      _    <- updateAsterisms(pi, List(oid), List.empty, List(btid), List((oid, List(tid))))
      _ <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _ <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blind Offset Star".some, true)
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some, true)
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
      _ <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blind Offset Star".some, true)
      _ <- expectBlindOffsetFields(pi, oid2, false, none, none, false)
    } yield () 

  test("adding blind offset to an observation asterism via SQL is an error"):
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
        expected = expectedListResults("updateObservations", "observations", "Regular Target", true, "Blind Offset Star".some, true)
      )
      btid <- getBlindOffsetId(pi, oid)
      oid2 <- createObservationAs(pi, pid)
      _    <- insertIntoAstersimViaSqlAndExpectError(pid, oid2, btid)
      _    <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blind Offset Star".some, true)
      _    <- expectBlindOffsetFields(pi, oid2, false, none, none, false)
    } yield () 

  test("hard delete of blind offset target resets observation values"):
    for {
      pid  <- createProgramAs(pi)
      tid1 <- createTargetAs(pi, pid, "Regular Target 1")
      tid2 <- createTargetAs(pi, pid, "Regular Target 2")
      (oid, btid)  <- createObservationWithBlindOffsetAs(pi, pid, "Blind Offset", tid1, tid2)
      _    <- expectTargetExistenceAs(pi, btid, shouldBePresent = true)
      _    <- hardDeleteTarget(btid)
      _    <- expectTargetNotFoundAs(pi, btid)
      _    <- expectAsterismTargetsAs(pi, oid, tid1, tid2) // asterism unchanged
      _    <- expectBlindOffsetFields(pi, oid, true, none, none, false) // useBlindOffset should still be true
    } yield ()

  test("setting blind offset target id to null deletes any assigned target"): // testing the trigger
    for {
      pid         <- createProgramAs(pi)
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blinded by the light") 
      _           <- withSession(
        _.execute(sql"""
          UPDATE t_observation
          SET c_blind_offset_target_id = null
          WHERE c_observation_id = $observation_id
        """.command)(oid)
      )
      _           <- expectBlindOffsetFields(pi, oid, true, none, none, true)
      _           <- expectTargetNotFoundAs(pi, btid)
    } yield ()

  test("cloning observation clones blind offset - explicit blind offset"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetAs(pi, pid, "Regular")
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blind Offset Star", tid)
      clone       <- cloneObservationAs(pi, oid)
      _           <- expectAsterismTargetsAs(pi, oid, tid)
      _           <- expectAsterismTargetsAs(pi, clone, tid)
      btidClone   <- getBlindOffsetId(pi, clone) 
      _            = assertNotEquals(btid, btidClone)
      _           <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blind Offset Star".some, true)
      _           <- expectBlindOffsetFields(pi, clone, true, btidClone.some, "Blind Offset Star".some, true)
    } yield ()

  test("cloning observation clones blind offset - automatic blind offset"):
    for {
      pid         <- createProgramAs(pi)
      tid         <- createTargetAs(pi, pid, "Regular")
      oid         <- createObservationAs(pi, pid, tid)
      _           <- updateBlindOffsetViaServiceAs(pi, pid, oid, "Blinding")
      btid        <- getBlindOffsetId(pi, oid)
      clone       <- cloneObservationAs(pi, oid)
      _           <- expectAsterismTargetsAs(pi, oid, tid)
      _           <- expectAsterismTargetsAs(pi, clone, tid)
      btidClone   <- getBlindOffsetId(pi, clone) 
      _            = assertNotEquals(btid, btidClone)
      _           <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blinding".some, false)
      _           <- expectBlindOffsetFields(pi, clone, true, btidClone.some, "Blinding".some, false)
    } yield ()

  test("cloning observation clones blind offset - with new asterism"):
    for {
      pid         <- createProgramAs(pi)
      tid1        <- createTargetAs(pi, pid, "Regular 1")
      tid2        <- createTargetAs(pi, pid, "Regular 2")
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blinded", tid1)
      clone       <- cloneObservationWithTargetEnvironment(pi, oid, s"{asterism: [${tid2.asJson}]}")
      _           <- expectAsterismTargetsAs(pi, oid, tid1)
      _           <- expectAsterismTargetsAs(pi, clone, tid2)
      btidClone   <- getBlindOffsetId(pi, clone) 
      _            = assertNotEquals(btid, btidClone)
      _           <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blinded".some, true)
      _           <- expectBlindOffsetFields(pi, clone, true, btidClone.some, "Blinded".some, true)
    } yield ()

  test("cloning observation clones blind offset - with new blind offset overriding original"):
    def targetEnv(astId: Target.Id) = s"""{
      ${blindOffsetTargetInput(blindOffsetInput("Newly Blinded", "0.0", "0.0"))}
      asterism: [${astId.asJson}]
    }"""
    for {
      pid         <- createProgramAs(pi)
      tid1        <- createTargetAs(pi, pid, "Regular 1")
      tid2        <- createTargetAs(pi, pid, "Regular 2")
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blinded", tid1)
      clone       <- cloneObservationWithTargetEnvironment(pi, oid, targetEnv(tid2))
      _           <- expectAsterismTargetsAs(pi, oid, tid1)
      _           <- expectAsterismTargetsAs(pi, clone, tid2)
      btidClone   <- getBlindOffsetId(pi, clone) 
      _            = assertNotEquals(btid, btidClone)
      _           <- expectBlindOffsetFields(pi, oid, true, btid.some, "Blinded".some, true)
      _           <- expectBlindOffsetFields(pi, clone, true, btidClone.some, "Newly Blinded".some, true)
    } yield ()

  // This does cause an exception to be thrown via skunk, but I don't know how to catch it because it is
  // a deferred constraint. Since we don't allow creating non-science targets via the API, this should
  // never happen in practice.
  // test("creating blind offset without obs is error"):
  //   for
  //     pid <- createProgramAs(pi)
  //     tid <- createTargetViaServiceAs(pi, pid, lucuma.core.enums.TargetDisposition.BlindOffset, none)
  //   yield ()

class blindOffsetTargetNotSharedTrigger extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2)

  // Running this test by itself works, but I think it borks skunk so following tests fail.
  // So, I put it in a test class by itself.
  test("blind offset targets cannot be shared"): // testing the trigger
    val gotExpected = for {
      pid         <- createProgramAs(pi)
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blinding")
      oid2        <- createObservationAs(pi, pid)
      r           <- withSession(
        _.execute(sql"""
          UPDATE t_observation
          SET c_blind_offset_target_id = $target_id
          WHERE c_observation_id = $observation_id
        """.command)(btid, oid2).void
      )
      .as(false)
      .recover:
        case e if e.getMessage.contains("Blind offset targets cannot be shared") => true
        case _ => false
    } yield r
    assertIOBoolean(gotExpected, "Expected error while attempting to share")

class blindOffsetTargetMustBeBlindOffsetTrigger extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2)

  // Running this test by itself works, but I think it borks skunk so following tests fail.
  // So, I put it in a test class by itself.
  test("assigned blind offset targets must actually be blind offsets"): // testing the trigger
    val gotExpected = for {
      pid         <- createProgramAs(pi)
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blinding")
      tid         <- createTargetAs(pi, pid, "Science!!!")
      r           <- withSession(
        _.execute(sql"""
          UPDATE t_observation
          SET c_blind_offset_target_id = $target_id
          WHERE c_observation_id = $observation_id
        """.command)(tid, oid).void
      )
      .as(false)
      .recover:
        case e if e.getMessage.contains("Blind offset targets must have a disposition of blind_offset") => true
        case _ => false
    } yield r
    assertIOBoolean(gotExpected, "Expected error while attempting to assign science as blind offset")

