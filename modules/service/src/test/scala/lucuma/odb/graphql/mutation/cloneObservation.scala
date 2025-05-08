// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Target

class cloneObservation extends OdbSuite {
  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)
  val staff   = TestUsers.Standard.staff(nextId, nextId)
  lazy val validUsers = List(pi, pi2, staff)

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
          exposureTimeMode {
            signalToNoise {
              value
            }
          }
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
        flamingos2LongSlit {
          disperser
          filter
          fpu
        }
      }
      targetEnvironment {
        asterism {
          id
        }
      }
      $TimingWindowsGraph
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

  test("cloned observation should appear next to its source (top level)") {
    for
      pid  <- createProgramAs(pi)
      oids <- createObservationAs(pi, pid).replicateA(3)
      coid <- cloneObservationAs(pi, oids(1))
      es   <- groupElementsAs(pi, pid, None)
    yield assertEquals(es, List(oids(0), oids(1), coid, oids(2)).map(_.asRight))
  }

  test("cloned observation should appear next to its source (in a group)") {
    for
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid, None, None)
      oids <- createObservationInGroupAs(pi, pid, Some(gid)).replicateA(3)
      coid <- cloneObservationAs(pi, oids(1))
      es   <- groupElementsAs(pi, pid, Some(gid))
    yield assertEquals(es, List(oids(0), oids(1), coid, oids(2)).map(_.asRight))
  }

  test("https://github.com/gemini-hlsw/lucuma-odb/issues/388") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).flatMap { oid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(input: {observationId: ${oid.asJson}}) {
                originalObservation {
                  id
                }
                newObservation {
                  id
                  timingWindows {
                    startUtc
                  }
                }
              }
            }
          """
        )
      }
    }
  }

  private def referenceTest(f: (Observation.Id, ObservationReference) => String): IO[Unit] =
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oref = ObservationReference(ref.get, PosInt.unsafeFrom(1))
      oid <- createObservationAs(pi, pid)
      jsn <- query(
          user = staff,
          query = s"""
            mutation {
              cloneObservation(input: {
                ${f(oid, oref)}
              }) {
                originalObservation { id }
                newObservation { id }
              }
            }
          """
        )
    } yield {
      assertEquals(
        jsn.hcursor.downFields("cloneObservation", "originalObservation", "id").require[Observation.Id],
        oid
      )
      assertNotEquals(
        jsn.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id],
        oid
      )
    }

  test("can specify the observation to clone using its reference") {
    referenceTest { (oid, ref) =>
      s"""
         observationReference: ${ref.asJson}
      """
    }
  }

  test("can specify the observation to clone using both its id and reference") {
    referenceTest { (oid, ref) =>
      s"""
         observationId: ${oid.asJson},
         observationReference: ${ref.asJson}
      """
    }
  }

  test("fail if no ids are provided") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oref = ObservationReference(ref.get, PosInt.unsafeFrom(1))
      oid <- createObservationAs(pi, pid)
      _   <- expect(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(input: {}) {
                originalObservation { id }
                newObservation { id }
              }
            }
          """,
          expected = List("One of observationId or observationReference must be provided.").asLeft
        )
    } yield ()
  }

  test("fail if the id and reference do not correspond") {
    for {
      pid <- createProgramAs(pi)
      ref <- setProgramReference(staff, pid, """calibration: { semester: "2025B", instrument: GMOS_SOUTH }""")
      oref = ObservationReference(ref.get, PosInt.unsafeFrom(1))
      oid <- createObservationAs(pi, pid)
      oidx = Observation.Id.fromLong(oid.value.value - 1).get
      _   <- expect(
          user = pi,
          query = s"""
            mutation {
              cloneObservation(input: {
                observationId: ${oidx.asJson},
                observationReference: ${oref.asJson}
              }) {
                newObservation { id }
              }
            }
          """,
          expected = List(s"Observation '${oref.label}' (id $oid) does not correspond to observation id $oidx.").asLeft
        )
    } yield ()
  }
}
