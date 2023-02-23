// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all._
import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ObsActiveStatus

class cloneObservation extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi, pi2)

  // N.B. this is copied from the test for Query/observation; if it doesn't end up changing we
  // should factor it out.
  def createObservation(pid: Program.Id, tid1: Target.Id, tid2: Target.Id) =
    query(
      user = pi,
      query = s"""
        mutation {
          createObservation(
            input: {
              programId: "$pid"
              SET: {
                constraintSet: {
                  cloudExtinction: POINT_ONE
                  imageQuality: POINT_ONE
                  skyBackground: DARKEST
                }
                targetEnvironment: { asterism: ["$tid1", "$tid2"] }
                scienceRequirements: {
                  mode: SPECTROSCOPY
                  spectroscopy: {
                    wavelength: { nanometers: 500 }
                    resolution: 100
                    signalToNoise: 100.0
                    wavelengthCoverage: { nanometers: 20 }
                    focalPlane: SINGLE_SLIT
                    focalPlaneAngle: { microarcseconds: 0 }
                  }
                }
                observingMode: {
                  gmosNorthLongSlit: {
                    grating: R831_G5302
                    filter: R_PRIME
                    fpu: LONG_SLIT_0_50
                    centralWavelength: { nanometers: 500 }
                  }
                }
              }
            }
          ) {
            observation {
              id
            }
          }
        }
      """
    ).map{ j => j.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id] }

  // properties we expect to be the same
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
      targetEnvironment { 
        asterism $FullTargetGraph
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
      }
    }
  """

  test("clones should have the same properties, including asterism") {
    createProgramAs(pi).flatMap { pid =>
      val t = createTargetAs(pi, pid)
      (t, t).tupled.flatMap { (tid1, tid2) =>
        createObservation(pid, tid1, tid2).flatMap { oid =>
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
            assertEquals(
              json.hcursor.downFields("cloneObservation", "originalObservation").require[Json],
              json.hcursor.downFields("cloneObservation", "newObservation").require[Json]
            )
          }
        }
      }
    }
  }

  test("clones should have different ids") {
    createProgramAs(pi).flatMap { pid =>
      val t = createTargetAs(pi, pid)
      (t, t).tupled.flatMap { (tid1, tid2) =>
        createObservation(pid, tid1, tid2).flatMap { oid =>
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

  test("cloned asterism should not include deleted targets") {

    val setup =
      for
        pid  <- createProgramAs(pi)
        tid1 <- createTargetAs(pi, pid)
        tid2 <- createTargetAs(pi, pid)
        oid1 <- createObservation(pid, tid1, tid2)
        _    <- updateTargetExistencetAs(pi, tid1, Existence.Deleted)
        oid2 <- cloneObservationAs(pi, oid1)
        _    <- updateTargetExistencetAs(pi, tid1, Existence.Present)
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