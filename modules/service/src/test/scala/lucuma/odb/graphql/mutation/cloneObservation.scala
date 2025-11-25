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
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
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
          offsets { arcseconds }
          explicitOffsets { arcseconds }
          defaultOffsets { arcseconds }
          spatialOffsets { arcseconds }
          explicitSpatialOffsets { arcseconds }
          defaultSpatialOffsets { arcseconds }
          acquisition {
            filter
            defaultFilter
            explicitFilter
            roi
            defaultRoi
            explicitRoi
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
          }
        }
        gmosSouthLongSlit {
          grating
          filter
          fpu
          centralWavelength { nanometers }
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
          offsets { arcseconds }
          explicitOffsets { arcseconds }
          defaultOffsets { arcseconds }
          spatialOffsets { arcseconds }
          explicitSpatialOffsets { arcseconds }
          defaultSpatialOffsets { arcseconds }
          acquisition {
            filter
            defaultFilter
            explicitFilter
            roi
            defaultRoi
            explicitRoi
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
          }
        }
        gmosNorthImaging {
          filters { filter }
          initialFilters { filter }
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
          filters { filter }
          initialFilters { filter }
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
          telluricType {
            tag
            starTypes
          }
          acquisition {
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

  def filtersQuery(n: String): String =
    s"""$n {
      filter
      exposureTimeMode {
        signalToNoise {
          value
          at { nanometers }
        }
      }
    }
    """

  def filterJson[L: Enumerated](filter: L, value: Int, nm: Int): Json =
    json"""
      {
        "filter": ${Enumerated[L].tag(filter).toScreamingSnakeCase.asJson},
        "exposureTimeMode": {
          "signalToNoise": {
            "value": ${BigDecimal((value * 1000).toLong, 3).asJson},
            "at": { "nanometers": ${BigDecimal((nm * 1000).toLong, 3).asJson} }
          }
        }
      }
    """

  test("clone GMOS North imaging observation preserves filters and configuration"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        createGmosNorthImagingObservationAs(pi, pid, tid).flatMap: oid =>
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
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
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
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
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
                          "filters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ],
                          "initialFilters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ],
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
                          "filters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ],
                          "initialFilters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ],
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

  test("clone GMOS North imaging observation makes independent filters"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        createGmosNorthImagingObservationAs(pi, pid, tid).flatMap: oid =>
          expect( // provide filters with distinct ETMs
            user     = pi,
            query    = s"""
              mutation {
                updateObservations(input: {
                  SET: {
                    observingMode: {
                      gmosNorthImaging: {
                        filters: [
                          {
                            filter: G_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 101.0
                                at: { nanometers: 501.0 }
                              }
                            }
                          },
                          {
                            filter: R_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 102.0
                                at: { nanometers: 502.0 }
                              }
                            }
                          },
                          {
                            filter: I_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 103.0
                                at: { nanometers: 503.0 }
                              }
                            }
                          },
                          {
                            filter: Z_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 104.0
                                at: { nanometers: 504.0 }
                              }
                            }
                          }
                        ]
                      }
                    }
                  }
                  WHERE: {
                    id: { EQ: ${oid.asJson} }
                  }
                }) {
                  observations {
                    observingMode {
                      gmosNorthImaging {
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
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
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": [
                            ${filterJson(GmosNorthFilter.GPrime, 101, 501)},
                            ${filterJson(GmosNorthFilter.RPrime, 102, 502)},
                            ${filterJson(GmosNorthFilter.IPrime, 103, 503)},
                            ${filterJson(GmosNorthFilter.ZPrime, 104, 504)}
                          ],
                          "initialFilters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ]
                        }
                      }
                    }
                  ]
                }
              }
            """.asRight
          ) *> expect(   // Clone
            user = pi,
            query = s"""
              mutation {
                cloneObservation(input: {
                  observationId: "$oid"
                }) {
                  originalObservation {
                    observingMode {
                      gmosNorthImaging {
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
                      }
                    }
                  }
                  newObservation {
                    observingMode {
                      gmosNorthImaging {
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
                      }
                    }
                  }
                }
              }
            """,
            expected = json"""
              {
                "cloneObservation": {
                  "originalObservation": {
                    "observingMode": {
                      "gmosNorthImaging": {
                        "filters": [
                          ${filterJson(GmosNorthFilter.GPrime, 101, 501)},
                          ${filterJson(GmosNorthFilter.RPrime, 102, 502)},
                          ${filterJson(GmosNorthFilter.IPrime, 103, 503)},
                          ${filterJson(GmosNorthFilter.ZPrime, 104, 504)}
                        ],
                        "initialFilters": [
                          ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                          ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                        ]
                      }
                    }
                  },
                  "newObservation": {
                    "observingMode": {
                      "gmosNorthImaging": {
                        "filters": [
                          ${filterJson(GmosNorthFilter.GPrime, 101, 501)},
                          ${filterJson(GmosNorthFilter.RPrime, 102, 502)},
                          ${filterJson(GmosNorthFilter.IPrime, 103, 503)},
                          ${filterJson(GmosNorthFilter.ZPrime, 104, 504)}
                        ],
                        "initialFilters": [
                          ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                          ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                        ]
                      }
                    }
                  }
                }
              }
            """.asRight
          ) *> expect(  // mutate cloned observation
            user     = pi,
            query    = s"""
              mutation {
                updateObservations(input: {
                  SET: {
                    observingMode: {
                      gmosNorthImaging: {
                        filters: [
                          {
                            filter: G_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 105.0
                                at: { nanometers: 505.0 }
                              }
                            }
                          },
                          {
                            filter: R_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 106.0
                                at: { nanometers: 506.0 }
                              }
                            }
                          },
                          {
                            filter: I_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 107.0
                                at: { nanometers: 507.0 }
                              }
                            }
                          },
                          {
                            filter: Z_PRIME,
                            exposureTimeMode: {
                              signalToNoise: {
                                value: 108.0
                                at: { nanometers: 508.0 }
                              }
                            }
                          }
                        ]
                      }
                    }
                  }
                  WHERE: {
                    id: { EQ: ${Gid[Observation.Id].partialNext(oid).get.asJson} }
                  }
                }) {
                  observations {
                    observingMode {
                      gmosNorthImaging {
                        ${filtersQuery("filters")}
                        ${filtersQuery("initialFilters")}
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
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": [
                            ${filterJson(GmosNorthFilter.GPrime, 105, 505)},
                            ${filterJson(GmosNorthFilter.RPrime, 106, 506)},
                            ${filterJson(GmosNorthFilter.IPrime, 107, 507)},
                            ${filterJson(GmosNorthFilter.ZPrime, 108, 508)}
                          ],
                          "initialFilters": [
                            ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                            ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                          ]
                        }
                      }
                    }
                  ]
                }
              }
            """.asRight
          ) *> expect(  // original observation unmodified
            user     = pi,
            query    = s"""
              query {
                observation(observationId: "$oid") {
                  observingMode {
                    gmosNorthImaging {
                      ${filtersQuery("filters")}
                      ${filtersQuery("initialFilters")}
                    }
                  }
                }
              }
            """,
            expected = json"""
              {
                "observation": {
                  "observingMode": {
                    "gmosNorthImaging": {
                      "filters": [
                        ${filterJson(GmosNorthFilter.GPrime, 101, 501)},
                        ${filterJson(GmosNorthFilter.RPrime, 102, 502)},
                        ${filterJson(GmosNorthFilter.IPrime, 103, 503)},
                        ${filterJson(GmosNorthFilter.ZPrime, 104, 504)}
                      ],
                      "initialFilters": [
                        ${filterJson(GmosNorthFilter.GPrime, 100, 1210)},
                        ${filterJson(GmosNorthFilter.RPrime, 100, 1210)}
                      ]
                    }
                  }
                }
              }
            """.asRight
          )

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
                        filters { filter }
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
                        filters { filter }
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
                          "filters": [
                            { "filter": "G_PRIME" },
                            { "filter": "R_PRIME" }
                          ],
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
                          "filters": [
                            { "filter": "G_PRIME" },
                            { "filter": "R_PRIME" }
                          ],
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

  test("close GMOS imaging observation preserves offset generator"):
    val inputsAndResults = List(
      s"""
        {
          enumerated: {
            values: [
              {
                offset: {
                  p: { arcseconds: 10.0 }
                  q: { arcseconds: 11.0 }
                }
                guiding: ENABLED
              },
              {
                offset: {
                  p: { arcseconds: 12.0 }
                  q: { arcseconds: 13.0 }
                }
                guiding: ENABLED
              }
            ]
          }
        }
      """ -> json"""
        {
          "enumerated": {
            "values": [
              {
                "offset": {
                  "p": { "arcseconds": 10 },
                  "q": { "arcseconds": 11 }
                },
                "guiding": "ENABLED"
              },
              {
                "offset": {
                  "p": { "arcseconds": 12 },
                  "q": { "arcseconds": 13 }
                },
                "guiding": "ENABLED"
              }
            ]
          },
          "grid": null,
          "random": null,
          "spiral": null
        }
      """,
      s"""
        {
          grid: {
            cornerA: {
              p: { arcseconds: 14.0 }
              q: { arcseconds: 15.0 }
            }
            cornerB: {
              p: { arcseconds: 16.0 }
              q: { arcseconds: 17.0 }
            }
          }
        }
      """ -> json"""
        {
          "enumerated": null,
          "grid": {
            "cornerA": {
              "p": { "arcseconds":  14 },
              "q": { "arcseconds":  15 }
            },
            "cornerB": {
              "p": { "arcseconds":  16 },
              "q": { "arcseconds":  17 }
            }
          },
          "random": null,
          "spiral": null
        }
      """,
      s"""
        {
          random: {
            size: { arcseconds: 18.0 }
            center: {
              p: { arcseconds: 19.0 }
              q: { arcseconds: 20.0 }
            }
          }
        }
      """ -> json"""
        {
          "enumerated": null,
          "grid": null,
          "random": {
            "size": { "arcseconds": 18 },
            "center": {
              "p": { "arcseconds": 19 },
              "q": { "arcseconds": 20 }
            }
          },
          "spiral": null
        }
      """,
      s"""
        {
          spiral: {
            size: { arcseconds: 21.0 }
            center: {
              p: { arcseconds: 22.0 }
              q: { arcseconds: 23.0 }
            }
          }
        }
      """ -> json"""
        {
          "enumerated": null,
          "grid": null,
          "random": null,
          "spiral": {
            "size": { "arcseconds": 21 },
            "center": {
              "p": { "arcseconds": 22 },
              "q": { "arcseconds": 23 }
            }
          }
        }
      """
    )

    def createObservation(
      pid: Program.Id,
      tid: Target.Id,
      obj: String,
      sky: String
    ): IO[Observation.Id] =
      query(
        user  = pi,
        query =
          s"""
            mutation {
              createObservation(input: {
              programId: "$pid",
                SET: {
                  targetEnvironment: {
                    asterism: ["$tid"]
                  }
                  scienceRequirements: ${scienceRequirementsObject(ObservingModeType.GmosNorthImaging)}
                  observingMode: {
                    gmosNorthImaging: {
                      filters: [
                        {
                          filter: R_PRIME
                        },
                        {
                          filter: G_PRIME
                        }
                      ]
                      objectOffsetGenerator: $obj
                      skyOffsetGenerator: $sky
                    }
                  }
                  constraintSet: {
                    imageQuality: ${ImageQuality.Preset.PointEight.tag.toScreamingSnakeCase}
                  }
                }
              }) {
                observation { id }
              }
            }
          """
        ).map: json =>
          json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]

    inputsAndResults.zip(inputsAndResults.tail).traverse:
      case ((objIn, objRes), (skyIn, skyRes)) =>
        createProgramAs(pi).flatMap: pid =>
          createTargetAs(pi, pid).flatMap: tid =>
            createObservation(pid, tid, objIn, skyIn).flatMap: oid =>
              expect(
                user  = pi,
                query = s"""
                  mutation {
                    cloneObservation(input: {
                      observationId: "$oid"
                    }) {
                      newObservation {
                        observingMode {
                          gmosNorthImaging {
                            objectOffsetGenerator {
                              enumerated {
                                values {
                                  offset {
                                    p { arcseconds }
                                    q { arcseconds }
                                  }
                                  guiding
                                }
                              }
                              grid {
                                cornerA {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                                cornerB {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                              random {
                                size { arcseconds }
                                center {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                              spiral {
                                size { arcseconds }
                                center {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                            }
                            skyOffsetGenerator {
                              enumerated {
                                values {
                                  offset {
                                    p { arcseconds }
                                    q { arcseconds }
                                  }
                                  guiding
                                }
                              }
                              grid {
                                cornerA {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                                cornerB {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                              random {
                                size { arcseconds }
                                center {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                              spiral {
                                size { arcseconds }
                                center {
                                  p { arcseconds }
                                  q { arcseconds }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                """,
                expected = json"""
                  {
                    "cloneObservation": {
                      "newObservation": {
                        "observingMode": {
                          "gmosNorthImaging": {
                            "objectOffsetGenerator": $objRes,
                            "skyOffsetGenerator": $skyRes
                          }
                        }
                      }
                    }
                  }
                """.asRight
              )

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
              exposureTimeMode: {
                signalToNoise: {
                  value: 100.0
                  at: { nanometers: 543.21 }
                }
              }
              explicitYBin: TWO
              acquisition: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: 99.9
                    at: { nanometers: 123.45 }
                  }
                }
              }
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
