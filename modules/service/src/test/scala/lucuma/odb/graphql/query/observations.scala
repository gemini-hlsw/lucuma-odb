// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput

class observations extends OdbSuite {

  val pi      = TestUsers.Standard.pi(nextId, nextId)
  val pi2     = TestUsers.Standard.pi(nextId, nextId)
  val pi3     = TestUsers.Standard.pi(nextId, nextId)
  val pi4     = TestUsers.Standard.pi(nextId, nextId)
  val service = TestUsers.service(nextId)
  val staff   = TestUsers.Standard.staff(nextId, nextId)

  val validUsers = List(pi, pi2, pi3, pi4, service, staff).toList

  test("simple observation selection") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid).replicateA(5).flatMap { oids =>
        expect(
          user = pi,
          query = s"""
            query {
              observations() {
                hasMore
                matches {
                  id
                  calibrationRole
                  observerNotes
                  useBlindOffset
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "observations" -> Json.obj(
                "hasMore" -> Json.False,
                "matches" -> Json.fromValues(
                    oids.map { id =>
                      Json.obj(
                        "id"              -> id.asJson,
                        "calibrationRole" -> Json.Null,
                        "observerNotes"   -> Json.Null,
                        "useBlindOffset"  -> Json.False
                      )
                    }
                )
              )
            )
          )
        )
      }
    }
  }

  test("simple observation selection with limit") {
    createProgramAs(pi2).flatMap { pid =>
      createObservationAs(pi2, pid).replicateA(5).flatMap { oids =>
        expect(
          user = pi2,
          query = s"""
            query {
              observations(LIMIT: 3) {
                hasMore
                matches {
                  id
                  useBlindOffset
                }
              }
            }
          """,
          expected =
            Right(Json.obj(
              "observations" -> Json.obj(
                "hasMore" -> Json.True,
                "matches" -> Json.fromValues(
                    oids.take(3).map { id =>
                      Json.obj(
                        "id" -> id.asJson,
                        "useBlindOffset" -> Json.False
                      )
                    }
                )
              )
            )
          )
        )
      }
    }
  }

  test("simple observation where selection") {
    createProgramAs(pi2).flatMap { pid =>
      createObservationAs(pi2, pid).flatMap { oid =>
        expect(
          user = pi2,
          query = s"""
            query {
              observations(WHERE: { id: { EQ: "$oid" }}) {
                hasMore
                matches {
                  id
                  useBlindOffset
                }
              }
            }
          """,
        expected =
          Right(
            json"""
              {
                "observations" : {
                  "hasMore" : false,
                  "matches" : [
                    {
                      "id" : $oid,
                      "useBlindOffset" : false
                    }
                  ]
                }
              }
            """
          )
        )
      }
    }
  }

  test("select science bands") {
    val allocs = List(
      AllocationInput(TimeAccountingCategory.CA, ScienceBand.Band1, 1.hourTimeSpan),
      AllocationInput(TimeAccountingCategory.CL, ScienceBand.Band2, 10.minTimeSpan)
    )

    for {
      pid  <- createProgramAs(pi2)
      _    <- setAllocationsAs(staff, pid, allocs)
      oid1 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid1, ScienceBand.Band1.some)
      oid2 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid2, ScienceBand.Band1.some)
      oid3 <- createObservationAs(pi2, pid)
      _    <- setScienceBandAs(pi2, oid3, ScienceBand.Band2.some)
      oid4 <- createObservationAs(pi2, pid)
      b1   <- observationsWhere(pi2, """scienceBand: { EQ: BAND1 }""")
      b2   <- observationsWhere(pi2, """scienceBand: { EQ: BAND2 }""")
      b3   <- observationsWhere(pi2, """scienceBand: { EQ: BAND3 }""")
      bn   <- observationsWhere(pi2, s"""program: { id: { EQ: "$pid" } }, scienceBand: { IS_NULL: true }""")
      bs   <- observationsWhere(pi2, "scienceBand: { IS_NULL: false }")
    } yield {
      assertEquals(b1, List(oid1, oid2))
      assertEquals(b2, List(oid3))
      assertEquals(b3, Nil)
      assertEquals(bn, List(oid4))
      assertEquals(bs, List(oid1, oid2, oid3))
    }
  }

  def createObservationWithNullSpecRequirements(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  spectroscopy: null
                }
              }
            }) {
              observation {
                id
              }
            }
          }
          """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createObservationWithDefinedSpecRequirements(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 100.0
                      at: { angstroms: 71 }
                    }
                  }
                  spectroscopy: {
                    wavelength: {
                      angstroms: 42
                    }
                    wavelengthCoverage: {
                      angstroms: 99
                    }
                    focalPlaneAngle: {
                      arcseconds: 666
                    }
                  }
                }
              }
            }) {
              observation {
                id
              }
            }
          }
          """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createObservationWithDefinedImgRequirements(user: User, pid: Program.Id): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 100.0
                      at: { angstroms: 71 }
                    }
                  }
                  spectroscopy: {
                    wavelength: {
                      angstroms: 42
                    }
                    wavelengthCoverage: {
                      angstroms: 99
                    }
                    focalPlaneAngle: {
                      arcseconds: 666
                    }
                  }
                }
              }
            }) {
              observation {
                id
              }
            }
          }
          """
    ) map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  test("select observations with science requirements containing null and non-null embeds") {
    createProgramAs(pi).flatMap { pid =>
      (createObservationWithDefinedSpecRequirements(pi, pid), createObservationWithNullSpecRequirements(pi, pid))
        .tupled
        .flatMap { (oid1, oid2) =>
          expect(
            user = pi,
            query = s"""
              query {
                observations(WHERE: {
                  program: {
                    id: { EQ: "$pid" }
                  }
                }) {
                  matches {
                    id
                    scienceRequirements {
                      exposureTimeMode {
                        signalToNoise {
                          at { picometers }
                        }
                      }
                      spectroscopy {
                        wavelength {
                          picometers
                        }
                        wavelengthCoverage {
                          picometers
                        }
                        focalPlaneAngle {
                          milliarcseconds
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
                "observations" : {
                  "matches" : [
                    {
                      "id" : $oid1,
                      "scienceRequirements" : {
                        "exposureTimeMode": {
                          "signalToNoise": {
                            "at": {
                              "picometers": 7100
                            }
                          }
                        },
                        "spectroscopy" : {
                          "wavelength" : {
                            "picometers" : 4200
                          },
                          "wavelengthCoverage" : {
                            "picometers" : 9900
                          },
                          "focalPlaneAngle" : {
                            "milliarcseconds" : 666000
                          }
                        }
                      }
                    },
                    {
                      "id" : $oid2,
                      "scienceRequirements" : {
                        "exposureTimeMode" : null,
                        "spectroscopy" : null
                      }
                    }
                  ]
                }
              }
              """
            )
          )
        }
    }
  }

  test("select group info on observation without a group") {
    for {
      pid <- createProgramAs(pi2)
      oid <- createObservationAs(pi2, pid)
      _   <- expect(
               user = pi2,
               query = s"""
              query {
                observations(WHERE: { id: { EQ: "$oid" }}) {
                  hasMore
                  matches {
                    id
                    groupId
                    groupIndex
                    useBlindOffset
                  }
                }
              }""",
               expected = Right(json"""
                        {
                          "observations" : {
                            "hasMore" : false,
                            "matches" : [
                              {
                                "id" : $oid,
                                "groupId" : null,
                                "groupIndex" : 0,
                                "useBlindOffset" : false
                              }
                            ]
                          }
                        }""")
             )
    } yield ()
  }

  test("select group info on observation with a group") {
    for {
      pid <- createProgramAs(pi2)
      gid <- createGroupAs(pi2, pid)
      oid <- createObservationInGroupAs(pi2, pid, gid.some)
      _   <- expect(
               user = pi2,
               query = s"""
              query {
                observations(WHERE: { id: { EQ: "$oid" }}) {
                  hasMore
                  matches {
                    id
                    groupId
                    groupIndex
                    useBlindOffset
                  }
                }
              }""",
               expected = Right(json"""
                        {
                          "observations" : {
                            "hasMore" : false,
                            "matches" : [
                              {
                                "id" : $oid,
                                "groupId" : $gid,
                                "groupIndex" : 0,
                                "useBlindOffset" : false
                              }
                            ]
                          }
                        }
                      """)
             )

    } yield ()
  }

  test("filter on site"):
    for
      pid     <- createProgramAs(pi3)
      oid1    <- createObservationAs(pi3, pid, ObservingModeType.GmosNorthLongSlit.some)
      oid2    <- createObservationAs(pi3, pid, ObservingModeType.GmosSouthLongSlit.some)
      oid3    <- createObservationAs(pi3, pid)
      oid4    <- createObservationAs(pi3, pid, ObservingModeType.GmosNorthImaging.some)
      oid5    <- createObservationAs(pi3, pid, ObservingModeType.GmosSouthImaging.some)
      gn      <- observationsWhere(pi3, """site: { EQ: GN }""")
      gs      <- observationsWhere(pi3, """site: { EQ: GS }""")
      both    <- observationsWhere(pi3, """site: { IN: [ GN, GS] }""")
      isNull  <- observationsWhere(pi3, """site: { IS_NULL: true }""")
      notNull <- observationsWhere(pi3, """site: { IS_NULL: false }""")
    yield
      assertEquals(gn, List(oid1, oid4))
      assertEquals(gs, List(oid2, oid5))
      assertEquals(both,    List(oid1, oid2, oid4, oid5))
      assertEquals(isNull,  List(oid3))
      assertEquals(notNull, List(oid1, oid2, oid4, oid5))

  test("filter on instrument"):
    for
      pid     <- createProgramAs(pi4)
      oid1    <- createObservationAs(pi4, pid, ObservingModeType.GmosNorthLongSlit.some)
      oid2    <- createObservationAs(pi4, pid, ObservingModeType.GmosSouthLongSlit.some)
      oid3    <- createObservationAs(pi4, pid)
      oid4    <- createObservationAs(pi4, pid, ObservingModeType.GmosNorthImaging.some)
      oid5    <- createObservationAs(pi4, pid, ObservingModeType.GmosSouthImaging.some)
      gn      <- observationsWhere(pi4, """instrument: { EQ: GMOS_NORTH }""")
      gs      <- observationsWhere(pi4, """instrument: { EQ: GMOS_SOUTH }""")
      both    <- observationsWhere(pi4, """instrument: { IN: [ GMOS_NORTH, GMOS_SOUTH ] }""")
      isNull  <- observationsWhere(pi4, """instrument: { IS_NULL: true }""")
      notNull <- observationsWhere(pi4, """instrument: { IS_NULL: false }""")
    yield
      assertEquals(gn, List(oid1, oid4))
      assertEquals(gs, List(oid2, oid5))
      assertEquals(both,    List(oid1, oid2, oid4, oid5))
      assertEquals(isNull,  List(oid3))
      assertEquals(notNull, List(oid1, oid2, oid4, oid5))

  test("conflicting filter"):
    for
      pid  <- createProgramAs(pi4)
      oid1 <- createObservationAs(pi4, pid, ObservingModeType.GmosNorthLongSlit.some)
      oid2 <- createObservationAs(pi4, pid, ObservingModeType.GmosSouthLongSlit.some)
      res  <- observationsWhere(pi4, """instrument: { EQ: GMOS_NORTH }, site: { EQ: GS }""")
    yield assertEquals(res, Nil)

  test("query GMOS North imaging observations"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      _   <- expect(
                user = pi,
                query = s"""
                  query {
                    observations(WHERE: {
                      instrument: { EQ: GMOS_NORTH }
                    }) {
                      matches {
                        id
                        useBlindOffset
                        observingMode {
                          gmosNorthImaging {
                            filters
                            initialFilters
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
                      "observations": {
                        "matches": [
                          {
                            "id": $oid,
                            "useBlindOffset": false,
                            "observingMode": {
                              "gmosNorthImaging": {
                                "filters": ["G_PRIME", "R_PRIME"],
                                "initialFilters": ["G_PRIME", "R_PRIME"],
                                "bin": "TWO",
                                "ampReadMode": "SLOW",
                                "ampGain": "LOW",
                                "roi": "FULL_FRAME"
                              }
                            }
                          }
                        ]
                      }
                    }
                  """
                )
              )
    } yield ()

  test("query GMOS South imaging observations"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosSouthImagingObservationAs(pi, pid, tid)
      _   <- expect(
                user = pi,
                query = s"""
                  query {
                    observations(WHERE: {
                      instrument: { EQ: GMOS_SOUTH }
                    }) {
                      matches {
                        id
                        useBlindOffset
                        observingMode {
                          gmosSouthImaging {
                            filters
                            initialFilters
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
                      "observations": {
                        "matches": [
                          {
                            "id": $oid,
                            "useBlindOffset": false,
                            "observingMode": {
                              "gmosSouthImaging": {
                                "filters": ["G_PRIME", "R_PRIME"],
                                "initialFilters": ["G_PRIME", "R_PRIME"],
                                "bin": "TWO",
                                "ampReadMode": "SLOW",
                                "ampGain": "LOW",
                                "roi": "FULL_FRAME"
                              }
                            }
                          }
                        ]
                      }
                    }
                  """
                )
              )
    } yield ()

  test("filter observations by instrument - GMOS imaging"):
    for {
      pid          <- createProgramAs(pi)
      tid          <- createTargetAs(pi, pid)
      oidGNI       <- createGmosNorthImagingObservationAs(pi, pid, tid)
      oidGSI       <- createGmosSouthImagingObservationAs(pi, pid, tid)
      oidGNLS      <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      oidGSLS      <- createObservationAs(pi, pid, ObservingModeType.GmosSouthLongSlit.some, tid)
      oidNone      <- createObservationAs(pi, pid, tid)
      gnObs        <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, instrument: { EQ: GMOS_NORTH }""")
      gsObs        <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, instrument: { EQ: GMOS_SOUTH }""")
      allGmos      <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, instrument: { IN: [ GMOS_NORTH, GMOS_SOUTH ] }""")
      noInstrument <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, instrument: { IS_NULL: true }""")
    } yield
      assertEquals(gnObs, List(oidGNI, oidGNLS))
      assertEquals(gsObs, List(oidGSI, oidGSLS))
      assertEquals(allGmos, List(oidGNI, oidGSI, oidGNLS, oidGSLS))
      assertEquals(noInstrument, List(oidNone))

  test("query GMOS imaging observations with complete configuration"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      (oidNorth, oidSouth) <-
        (createGmosNorthImagingObservationAs(pi, pid, tid), createGmosSouthImagingObservationAs(pi, pid, tid))
          .tupled
      _  <- expect(
              user = pi,
              query = s"""
                query {
                  observations(WHERE: {
                    program: { id: { EQ: "$pid" } }
                    instrument: { IN: [ GMOS_NORTH, GMOS_SOUTH ] }
                  }) {
                    matches {
                      id
                      instrument
                      useBlindOffset
                      observingMode {
                        mode
                        gmosNorthImaging {
                          filters
                          initialFilters
                          bin
                          ampReadMode
                          ampGain
                          roi
                        }
                        gmosSouthImaging {
                          filters
                          initialFilters
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
                    "observations": {
                      "matches": [
                        {
                          "id": $oidNorth,
                          "instrument": "GMOS_NORTH",
                          "useBlindOffset": false,
                          "observingMode": {
                            "mode": "GMOS_NORTH_IMAGING",
                            "gmosNorthImaging": {
                              "filters": ["G_PRIME", "R_PRIME"],
                              "initialFilters": ["G_PRIME", "R_PRIME"],
                              "bin": "TWO",
                              "ampReadMode": "SLOW",
                              "ampGain": "LOW",
                              "roi": "FULL_FRAME"
                            },
                            "gmosSouthImaging": null
                          }
                        },
                        {
                          "id": $oidSouth,
                          "instrument": "GMOS_SOUTH",
                          "useBlindOffset": false,
                          "observingMode": {
                            "mode": "GMOS_SOUTH_IMAGING",
                            "gmosNorthImaging": null,
                            "gmosSouthImaging": {
                              "filters": ["G_PRIME", "R_PRIME"],
                              "initialFilters": ["G_PRIME", "R_PRIME"],
                              "bin": "TWO",
                              "ampReadMode": "SLOW",
                              "ampGain": "LOW",
                              "roi": "FULL_FRAME"
                            }
                          }
                        }
                      ]
                    }
                  }
                """
              )
            )
    } yield ()

  test("initialFilters preserves original values when filters are updated"):
    for {
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
      _ <- expect(
        user = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              observingMode {
                gmosNorthImaging {
                  filters
                  initialFilters
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "filters": ["G_PRIME", "R_PRIME"],
                    "initialFilters": ["G_PRIME", "R_PRIME"]
                  }
                }
              }
            }
          """
        )
      )
      _ <- expect(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: {
                observingMode: {
                  gmosNorthImaging: {
                    filters: [I_PRIME, Z_PRIME]
                  }
                }
              }
            }) {
              observations {
                id
                observingMode {
                  gmosNorthImaging {
                    filters
                    initialFilters
                  }
                }
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updateObservations": {
                "observations": [
                  {
                    "id": $oid,
                    "observingMode": {
                      "gmosNorthImaging": {
                        "filters": ["I_PRIME", "Z_PRIME"],
                        "initialFilters": ["G_PRIME", "R_PRIME"]
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )
    } yield ()

}
