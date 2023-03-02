// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.odb.data.ObservingModeType

class cloneObservation extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi, pi2)

  // N.B. if we include the asterism here we hit a Grakle bug that's not yet minimized
  // see https://github.com/gemini-hlsw/lucuma-odb/issues/296
  val ObservationGraph = s"""
    { 
      title
      subtitle
      program { id }
      constraintSet {
        cloudExtinction
        imageQuality
        skyBackground
      }
      scienceRequirements {
        mode
        spectroscopy {
          wavelength { nanometers }
          resolution
          signalToNoise
          wavelengthCoverage { nanometers }
          focalPlane
          focalPlaneAngle { microarcseconds }
        }
      }
      observingMode {
        gmosNorthLongSlit {
          grating 
          filter 
          fpu 
          centralWavelength { nanometers }
        }
        gmosSouthLongSlit {
          grating 
          filter 
          fpu 
          centralWavelength { nanometers }
        }
      }
    }
    """

  test("clones should have the same properties, for all observing modes") {
    ObservingModeType.values.toList.traverse { obsMode =>
      createProgramAs(pi).flatMap { pid =>
        val t = createTargetAs(pi, pid)
        (t, t).tupled.flatMap { (tid1, tid2) =>
          createObservationAs(pi, pid, Some(obsMode), tid1, tid2).flatMap { oid =>
            query(
              user = pi,
              query = s"""
                mutation {
                  cloneObservation(input: {
                    observationId: "$oid"
                  }) {
                    originalObservation $ObservationGraph
                    newObservation $ObservationGraph
                  }
                }
              """
            ).map { json =>
              val a = json.hcursor.downFields("cloneObservation", "originalObservation").require[Json]
              val b = json.hcursor.downFields("cloneObservation", "newObservation").require[Json]
              assertEquals(a, b)
            }
          }          
        }
      }
    }      
  }

  test("clones should have different ids") {
    createProgramAs(pi).flatMap { pid =>
      val t = createTargetAs(pi, pid)
      (t, t).tupled.flatMap { (tid1, tid2) =>
        createObservationAs(pi, pid, tid1, tid2).flatMap { oid =>
          query(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation { id }
                  newObservation { id }
                }
              }
            """
          ).map { json =>
            assertNotEquals(
              json.hcursor.downFields("cloneObservation", "originalObservation", "id").require[Observation.Id],
              json.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id]
            )          
          }
        }
      }
    }
  }

  test("cloned observation should have the same asterism") {

    val setup =
      for
        pid  <- createProgramAs(pi)
        tid1 <- createTargetAs(pi, pid)
        tid2 <- createTargetAs(pi, pid)
        oid1 <- createObservationAs(pi, pid, tid1, tid2)
        oid2 <- cloneObservationAs(pi, oid1)
      yield (tid1, tid2, oid2)

    setup.flatMap { (tid1, tid2, oid) =>
      expect(
        user = pi,
        query = 
          s"""
          query {
          observation(observationId: ${oid.asJson}) {
              id
              targetEnvironment {
                asterism {
                  id
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "observation" : {
                "id" : $oid,
                "targetEnvironment" : {
                  "asterism" : [
                    {
                      "id" : $tid1
                    },
                    {
                      "id" : $tid2
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }

  }

  test("cloned asterism should not include deleted targets") {

    val setup =
      for
        pid  <- createProgramAs(pi)
        tid1 <- createTargetAs(pi, pid)
        tid2 <- createTargetAs(pi, pid)
        oid1 <- createObservationAs(pi, pid, tid1, tid2)
        _    <- deleteTargetAs(pi, tid1)
        oid2 <- cloneObservationAs(pi, oid1)
        _    <- undeleteTargetAs(pi, tid1)
      yield (tid2, oid2)

    setup.flatMap { (tid, oid) =>
      expect(
        user = pi,
        query = 
          s"""
          query {
          observation(observationId: ${oid.asJson}) {
              id
              targetEnvironment {
                asterism {
                  id
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "observation" : {
                "id" : $oid,
                "targetEnvironment" : {
                  "asterism" : [
                    {
                      "id" : $tid
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }

  }

  test("cloned observation should always be present/new/active") {

    def updateFields(pid: Program.Id, oid: Observation.Id): IO[Unit] =
      expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              programId: ${pid.asJson}
              SET: {
                existence: ${Existence.Deleted.tag.toUpperCase}
                status: ${ObsStatus.Observed.tag.toUpperCase}
                activeStatus: ${ObsActiveStatus.Inactive.tag.toUpperCase}
              },
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations {
                id
                existence
                status
                activeStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateObservations" : {
                "observations" : [
                  {
                    "id" : $oid,
                    "existence" : "DELETED",
                    "status" : "OBSERVED",
                    "activeStatus" : "INACTIVE"
                  }
                ]
              }
            }
          """        
        )
      )

    val setup =
      for
        pid <- createProgramAs(pi)
        oid <- createObservationAs(pi, pid)
        _   <- updateFields(pid, oid)
      yield oid

    setup.flatMap { oid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            cloneObservation(input: {
              observationId: "$oid"
            }) {
              newObservation {
                existence
                status
                activeStatus
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "cloneObservation" : {
                "newObservation" : {
                  "existence" : "PRESENT",
                  "status" : "NEW",
                  "activeStatus" : "ACTIVE"
                }
              }
            }
          """
        )
      )
    }

  }

}