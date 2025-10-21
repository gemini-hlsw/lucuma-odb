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
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Target
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class cloneObservation extends OdbSuite with ObservingModeSetupOperations {
  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)
  val staff   = TestUsers.Standard.staff(nextId, nextId)
  lazy val validUsers = List(pi, pi2, staff)

  val ObservationGraph = s"""
    {
      title
      subtitle
      program { id }
      observerNotes
      constraintSet {
        cloudExtinction
        imageQuality
        skyBackground
      }
      scienceRequirements {
        mode
        exposureTimeMode {
          signalToNoise {
            value
            at { nanometers }
          }
          timeAndCount {
            time { seconds }
            count
            at { nanometers }
          }
        }
        spectroscopy {
          wavelength { nanometers }
          resolution
          wavelengthCoverage { nanometers }
          focalPlane
          focalPlaneAngle { microarcseconds }
        }
        imaging {
          minimumFov { microarcseconds }
          narrowFilters
          broadFilters
          combinedFilters
        }
      }
      observingMode {
        gmosNorthLongSlit {
          grating
          filter
          fpu
          centralWavelength { nanometers }
          acquisitionExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          scienceExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          offsets { arcseconds }
          explicitOffsets { arcseconds }
          defaultOffsets { arcseconds }
          spatialOffsets { arcseconds }
          explicitSpatialOffsets { arcseconds }
          defaultSpatialOffsets { arcseconds }
        }
        gmosSouthLongSlit {
          grating
          filter
          fpu
          centralWavelength { nanometers }
          acquisitionExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          scienceExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          offsets { arcseconds }
          explicitOffsets { arcseconds }
          defaultOffsets { arcseconds }
          spatialOffsets { arcseconds }
          explicitSpatialOffsets { arcseconds }
          defaultSpatialOffsets { arcseconds }
        }
        gmosNorthImaging {
          filters
          initialFilters
          multipleFiltersMode
          bin
          ampReadMode
          ampGain
          roi
          offsets {
            p { arcseconds }
            q { arcseconds }
          }
        }
        gmosSouthImaging {
          filters
          initialFilters
          multipleFiltersMode
          bin
          ampReadMode
          ampGain
          roi
          offsets {
            p { arcseconds }
            q { arcseconds }
          }
        }
        flamingos2LongSlit {
          disperser
          filter
          fpu
          acquisitionExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          scienceExposureTimeMode {
            signalToNoise {
              value
              at { nanometers }
            }
            timeAndCount {
              time { seconds }
              count
              at { nanometers }
            }
          }
          telluricType {
            tag
            starTypes
          }
        }
      }
      targetEnvironment {
        useBlindOffset
        asterism {
          id
        }
        blindOffsetTarget {
          id
        }
        blindOffsetType
      }
      $TimingWindowsGraph
    }
    """

  val IsImplemented: Set[ObservingModeType] = ObservingModeType.values.toSet

  test("clones should have the same properties, for all observing modes") {
    ObservingModeType.values.toList.filter(IsImplemented.apply).traverse { obsMode =>
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
    referenceTest { (_, ref) =>
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

  test("clone GMOS North imaging observation preserves filters and configuration") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        createGmosNorthImagingObservationAs(pi, pid, tid).flatMap { oid =>
          expect(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation {
                    observingMode {
                      gmosNorthImaging {
                        filters
                        initialFilters
                        multipleFiltersMode
                        bin
                        ampReadMode
                        ampGain
                        roi
                      }
                    }
                  }
                  newObservation {
                    observingMode {
                      gmosNorthImaging {
                        filters
                        initialFilters
                        multipleFiltersMode
                        bin
                        ampReadMode
                        ampGain
                        roi
                      }
                    }
                  }
                }
              }
            """,
            expected = Right(
              json"""
                {
                  "cloneObservation": {
                    "originalObservation": {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": ["G_PRIME", "R_PRIME"],
                          "initialFilters": ["G_PRIME", "R_PRIME"],
                          "multipleFiltersMode": "GROUPED",
                          "bin": "TWO",
                          "ampReadMode": "SLOW",
                          "ampGain": "LOW",
                          "roi": "FULL_FRAME"
                        }
                      }
                    },
                    "newObservation": {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": ["G_PRIME", "R_PRIME"],
                          "initialFilters": ["G_PRIME", "R_PRIME"],
                          "multipleFiltersMode": "GROUPED",
                          "bin": "TWO",
                          "ampReadMode": "SLOW",
                          "ampGain": "LOW",
                          "roi": "FULL_FRAME"
                        }
                      }
                    }
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  test("clone GMOS South imaging observation preserves filters and configuration") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        createGmosSouthImagingObservationAs(pi, pid, tid).flatMap { oid =>
          expect(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation {
                    observingMode {
                      gmosSouthImaging {
                        filters
                        multipleFiltersMode
                        bin
                        ampReadMode
                        ampGain
                        roi
                      }
                    }
                  }
                  newObservation {
                    observingMode {
                      gmosSouthImaging {
                        filters
                        multipleFiltersMode
                        bin
                        ampReadMode
                        ampGain
                        roi
                      }
                    }
                  }
                }
              }
            """,
            expected = Right(
              json"""
                {
                  "cloneObservation": {
                    "originalObservation": {
                      "observingMode": {
                        "gmosSouthImaging": {
                          "filters": ["G_PRIME", "R_PRIME"],
                          "multipleFiltersMode": "GROUPED",
                          "bin": "TWO",
                          "ampReadMode": "SLOW",
                          "ampGain": "LOW",
                          "roi": "FULL_FRAME"
                        }
                      }
                    },
                    "newObservation": {
                      "observingMode": {
                        "gmosSouthImaging": {
                          "filters": ["G_PRIME", "R_PRIME"],
                          "multipleFiltersMode": "GROUPED",
                          "bin": "TWO",
                          "ampReadMode": "SLOW",
                          "ampGain": "LOW",
                          "roi": "FULL_FRAME"
                        }
                      }
                    }
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  test("clone GMOS imaging observation preserves spatial offsets") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid).flatMap { tid =>
        // Create a GMOS North imaging observation with spatial offsets using the simplified helper
        createGmosNorthImagingObservationAs(pi, pid, ImageQuality.Preset.PointEight, Some("""[
          { p: { arcseconds: "1.5" }, q: { arcseconds: "2.0" } },
          { p: { arcseconds: "-0.5" }, q: { arcseconds: "1.0" } }
        ]"""), tid).flatMap { oid =>
          // Now clone the observation and verify spatial offsets are preserved
          expect(
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation {
                    observingMode {
                      gmosNorthImaging {
                        offsets {
                          p { arcseconds }
                          q { arcseconds }
                        }
                      }
                    }
                  }
                  newObservation {
                    observingMode {
                      gmosNorthImaging {
                        offsets {
                          p { arcseconds }
                          q { arcseconds }
                        }
                      }
                    }
                  }
                }
              }
            """,
            expected = Right(
              json"""
                {
                  "cloneObservation": {
                    "originalObservation": {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "offsets": [
                            { "p": { "arcseconds": 1.500000 }, "q": { "arcseconds": 2.000000 } },
                            { "p": { "arcseconds": -0.500000 }, "q": { "arcseconds": 1.000000 } }
                          ]
                        }
                      }
                    },
                    "newObservation": {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "offsets": [
                            { "p": { "arcseconds": 1.500000 }, "q": { "arcseconds": 2.000000 } },
                            { "p": { "arcseconds": -0.500000 }, "q": { "arcseconds": 1.000000 } }
                          ]
                        }
                      }
                    }
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  test("clone Flamingos2 long slit observation preserves spatial offsets"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      // Create a Flamingos2 long slit observation with spatial offsets
      oid <- createFlamingos2LongSlitObservationAs(pi, pid, ImageQuality.Preset.PointEight, Some("""[
                { p: { arcseconds: 0.0 }, q: { arcseconds: 1.5 } },
                { p: { arcseconds: 0.0 }, q: { arcseconds: 0.5 } },
                { p: { arcseconds: 0.0 }, q: { arcseconds: 2.25 } },
                { p: { arcseconds: 0.0 }, q: { arcseconds: -1.0 } }
              ]"""), tid)
      // Update observation to set telluric type with star types
      _   <- query(
                user = pi,
                query = s"""
                          mutation {
                            updateObservations(input: {
                              SET: {
                                observingMode: {
                                  flamingos2LongSlit: {
                                    telluricType: {
                                      tag: MANUAL
                                      starTypes: ["A1", "A2"]
                                    }
                                  }
                                }
                              }
                              WHERE: {
                                id: { EQ: ${oid.asJson} }
                              }
                            }) {
                              observations {
                                id
                              }
                            }
                          }
                        """)
      _   <- expect(
              user = pi,
              query = s"""
                mutation {
                  cloneObservation(input: {
                    observationId: "$oid"
                  }) {
                    originalObservation {
                      observingMode {
                        flamingos2LongSlit {
                          offsets {
                            p {
                              arcseconds
                            }
                            q {
                              arcseconds
                            }
                          }
                          telluricType {
                            tag
                            starTypes
                          }
                        }
                      }
                    }
                    newObservation {
                      observingMode {
                        flamingos2LongSlit {
                          offsets {
                            p {
                              arcseconds
                            }
                            q {
                              arcseconds
                            }
                          }
                          telluricType {
                            tag
                            starTypes
                          }
                        }
                      }
                    }
                  }
                }
              """,
              expected = Right(
                json"""
                  {
                    "cloneObservation": {
                      "originalObservation": {
                        "observingMode": {
                          "flamingos2LongSlit": {
                            "offsets": [
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 1.500000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.500000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 2.250000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -1.000000 } }
                            ],
                            "telluricType": {
                              "tag": "MANUAL",
                              "starTypes": ["A1", "A2"]
                            }
                          }
                        }
                      },
                      "newObservation": {
                        "observingMode": {
                          "flamingos2LongSlit": {
                            "offsets": [
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 1.500000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.500000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 2.250000 } },
                              { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": -1.000000 } }
                            ],
                            "telluricType": {
                              "tag": "MANUAL",
                              "starTypes": ["A1", "A2"]
                            }
                          }
                        }
                      }
                    }
                  }
                """
              )
            )
      } yield ()

  test("clone observation should preserve observer notes"):
    for {
      pid    <- createProgramAs(pi)
      idjson <- query(
                  user = pi,
                  query = s"""
                    mutation {
                      createObservation(input: {
                        programId: "$pid"
                        SET: {
                          observerNotes: "Observation notes"
                          targetEnvironment: {
                            useBlindOffset: true
                          }
                        }
                      }) {
                        observation {
                          id
                        }
                      }
                    }
                  """
                )
      oid   = idjson.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      _     <- expect(
                user = pi,
                query = s"""
                  mutation {
                    cloneObservation(input: {
                      observationId: ${oid.asJson}
                    }) {
                      originalObservation {
                        observerNotes
                        targetEnvironment {
                          useBlindOffset
                        }
                      }
                      newObservation {
                        observerNotes
                        targetEnvironment {
                          useBlindOffset
                        }
                      }
                    }
                  }
                """,
                expected = Right(
                  json"""
                    {
                      "cloneObservation": {
                        "originalObservation": {
                          "observerNotes": "Observation notes",
                          "targetEnvironment": {
                            "useBlindOffset": true
                          }
                        },
                        "newObservation": {
                          "observerNotes": "Observation notes",
                          "targetEnvironment": {
                            "useBlindOffset": true
                          }
                        }
                      }
                    }
                  """
                )
              )
    } yield ()

  test("clone preserves exposure time modes"):
    createProgramAs(pi).flatMap: pid =>
      val t = createTargetAs(pi, pid)
      (t, t).tupled.flatMap: (tid1, tid2) =>
        createObservationWithModeAs(
          pi,
          pid,
          List(tid1, tid2),
          s"""
            gmosNorthLongSlit: {
              grating: R831_G5302
              filter: R_PRIME
              fpu: LONG_SLIT_0_50
              centralWavelength: {
                nanometers: 500
              }
              acquisitionExposureTimeMode: {
                signalToNoise: {
                  value: 99.9
                  at: { nanometers: 123.45 }
                }
              }
              scienceExposureTimeMode: {
                signalToNoise: {
                  value: 100.0
                  at: { nanometers: 543.21 }
                }
              }
              explicitYBin: TWO
            }
          """
        ).flatMap: oid =>
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
          ).map: json =>
            val a = json.hcursor.downFields("cloneObservation", "originalObservation").require[Json]
            val b = json.hcursor.downFields("cloneObservation", "newObservation").require[Json]
            assertEquals(a, b)

}
