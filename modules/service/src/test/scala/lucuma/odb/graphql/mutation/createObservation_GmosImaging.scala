// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.model.GuestUser
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User

class createObservation_GmosImaging extends OdbSuite:

  extension (ac: ACursor)
    def downPath(p: String*): ACursor =
      p.foldLeft(ac) { (aCursor, field) => aCursor.downField(field) }

    def liftIO[A: Decoder]: IO[A] =
      ac.as[A].leftMap(f => new RuntimeException(f.message)).liftTo[IO]

    def downIO[A: Decoder](p: String*): IO[A] =
      downPath(p*).liftIO[A]

  val pi: StandardUser     = TestUsers.Standard.pi(nextId, nextId)
  val pi2: StandardUser    = TestUsers.Standard.pi(nextId, nextId)
  val pi3: StandardUser    = TestUsers.Standard.pi(nextId, nextId)
  val ngo: StandardUser    = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff: StandardUser  = TestUsers.Standard.staff(nextId, nextId)
  val admin: StandardUser  = TestUsers.Standard.admin(nextId, nextId)
  val guest: GuestUser     = TestUsers.guest(nextId)
  val service: ServiceUser = TestUsers.service(nextId)

  lazy val validUsers: List[User] =
    List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  test("create GMOS North"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        createGmosNorthImagingObservationAs(pi, pid, tid).flatMap: oid =>
          expect(pi, s"""
            query {
              observation(observationId: "$oid") {
                observingMode {
                  gmosNorthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                        }
                      }
                    }
                    bin
                    ampReadMode
                    ampGain
                    roi
                  }
                }
              }
            }
          """, json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "variant": {
                      "interleaved": {
                        "filters": [
                          { "filter": "G_PRIME" },
                          { "filter": "R_PRIME" }
                        ]
                      }
                    },
                    "bin": "TWO",
                    "ampReadMode": "SLOW",
                    "ampGain": "LOW",
                    "roi": "FULL_FRAME"
                  }
                }
              }
            }
          """.asRight)

  test("create GMOS North with default binning of 1"):
    val gaussian: String = """
      sourceProfile: {
        gaussian: {
          fwhm: { arcseconds: 0.1 }
          spectralDefinition: {
            bandNormalized: {
              sed: {
                stellarLibrary: B5_III
              }
              brightnesses: [
                {
                  band: R
                  value: 15.0
                  units: VEGA_MAGNITUDE
                }
              ]
            }
          }
        }
      }
    """

    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid, sourceProfile = gaussian).flatMap: tid =>
        createGmosNorthImagingObservationAs(pi, pid, iq = ImageQuality.Preset.PointOne, tid).flatMap: oid =>
          expect(pi, s"""
            query {
              observation(observationId: "$oid") {
                observingMode {
                  gmosNorthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                        }
                      }
                    }
                    bin
                    ampReadMode
                    ampGain
                    roi
                  }
                }
              }
            }
          """, json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "variant": {
                      "interleaved": {
                        "filters": [
                          { "filter": "G_PRIME" },
                          { "filter": "R_PRIME" }
                        ]
                      }
                    },
                    "bin": "ONE",
                    "ampReadMode": "SLOW",
                    "ampGain": "LOW",
                    "roi": "FULL_FRAME"
                  }
                }
              }
            }
          """.asRight)

  test("create GMOS South"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        createGmosSouthImagingObservationAs(pi, pid, tid).flatMap: oid =>
          expect(pi, s"""
            query {
              observation(observationId: "$oid") {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                        }
                      }
                    }
                    bin
                    ampReadMode
                    ampGain
                    roi
                  }
                }
              }
            }
          """, json"""
            {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "variant": {
                      "interleaved": {
                        "filters": [
                          {
                            "filter": "G_PRIME"
                          },
                          {
                            "filter": "R_PRIME"
                          }
                        ]
                      }
                    },
                    "bin": "TWO",
                    "ampReadMode": "SLOW",
                    "ampGain": "LOW",
                    "roi": "FULL_FRAME"
                  }
                }
              }
            }
          """.asRight)

  test("have correct observing mode type"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        for
          oidN <- createGmosNorthImagingObservationAs(pi, pid, tid)
          oidS <- createGmosSouthImagingObservationAs(pi, pid, tid)
          result <- expect(pi, s"""
            query {
              observations(WHERE: { id: { IN: ["$oidN", "$oidS"] } }) {
                matches {
                  id
                  observingMode {
                    mode
                  }
                }
              }
            }
          """, json"""
            {
              "observations": {
                "matches": [
                  {
                    "id": $oidN,
                    "observingMode": {
                      "mode": "GMOS_NORTH_IMAGING"
                    }
                  },
                  {
                    "id": $oidS,
                    "observingMode": {
                      "mode": "GMOS_SOUTH_IMAGING"
                    }
                  }
                ]
              }
            }
          """.asRight)
        yield result


  test("cannot create GMOS North with empty filters"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosNorthImaging: {
                    variant: {
                      interleaved: {
                        filters: []
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """, List("Argument 'input.SET.observingMode.gmosNorthImaging.variant.interleaved' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft)

  test("cannot create GMOS South with empty filters"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    variant: {
                      interleaved: {
                        filters: []
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """, List("Argument 'input.SET.observingMode.gmosSouthImaging.variant.interleaved' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft)

  private def gmosImagingFilterTest(
    reqEtm:    Option[Int],
    gEtm:      Option[Int],
    rEtm:      Option[Int],
    gExpected: Json,
    rExpected: Json
  ) =
    def etmInput(etm: Option[Int]): String =
      etm.fold(""): sn =>
        s"""
          exposureTimeMode: {
            signalToNoise: {
              value: $sn
              at: { nanometers: 500.0 }
            }
          }
        """

    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  ${etmInput(reqEtm)}
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosNorthImaging: {
                    variant: {
                      interleaved: {
                        filters: [
                          {
                            filter: G_PRIME
                            ${etmInput(gEtm)}
                          },
                          {
                            filter: R_PRIME
                            ${etmInput(rEtm)}
                          }
                        ]
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosNorthImaging {
                    variant {
                      interleaved {
                        filters {
                          filter
                          exposureTimeMode {
                            signalToNoise {
                              value
                              at { nanometers }
                            }
                          }
                        }
                      }
                    }
                    initialFilters {
                      filter
                      exposureTimeMode {
                        signalToNoise {
                          value
                          at { nanometers }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosNorthImaging": {
                    "variant": {
                      "interleaved": {
                        "filters": [
                          ${gExpected},
                          ${rExpected}
                        ]
                      }
                    },
                    "initialFilters": [
                      ${gExpected},
                      ${rExpected}
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("can create GMOS North with differing explicit exposure time modes"):
    gmosImagingFilterTest(
      none,
      10.some,
      11.some,
      json"""
        {
          "filter": "G_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 10.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """,
      json"""
        {
          "filter": "R_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 11.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """
    )

  test("can create GMOS North with same explicit exposure time modes"):
    gmosImagingFilterTest(
      none,
      10.some,
      10.some,
      json"""
        {
          "filter": "G_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 10.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """,
      json"""
        {
          "filter": "R_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 10.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """
    )

  test("can create GMOS North with default + explicit exposure time modes"):
    gmosImagingFilterTest(
      11.some,
      none,
      10.some,
      json"""
        {
          "filter": "G_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 11.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """,
      json"""
        {
          "filter": "R_PRIME",
          "exposureTimeMode": {
            "signalToNoise": {
              "value": 10.000,
              "at": { "nanometers":  500.000 }
            }
          }
        }
      """
    )

  test("can create with uniform object and random sky offset generators"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 10.0
                      at: { nanometers: 500.0 }
                    }
                  }
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    variant: {
                      grouped: {
                        filters: [
                          { filter: G_PRIME },
                          { filter: R_PRIME }
                        ]
                        offsets: {
                          uniform: {
                            cornerA: {
                              p: { arcseconds: 10.0 }
                              q: { arcseconds: 11.0 }
                            }
                            cornerB: {
                              p: { arcseconds: 12.0 }
                              q: { arcseconds: 13.0 }
                            }
                          }
                        }
                        skyOffsets: {
                          random: {
                            size: { arcseconds: 14.0 }
                            center: {
                              p: { arcseconds: 15.0 }
                              q: { arcseconds: 16.0 }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      grouped {
                        offsets {
                          generatorType
                          enumerated {
                            values {
                              offset {
                                p { arcseconds }
                                q { arcseconds }
                              }
                              guiding
                            }
                          }
                          uniform {
                            cornerA {
                              p { arcseconds }
                              q { arcseconds }
                            }
                            cornerB {
                              p { arcseconds }
                              q { arcseconds }
                            }
                          }
                        }
                        skyOffsets {
                          generatorType
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
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "variant": {
                      "grouped": {
                        "offsets": {
                          "generatorType": "UNIFORM",
                          "enumerated": null,
                          "uniform": {
                            "cornerA": {
                              "p": { "arcseconds": 10 },
                              "q": { "arcseconds": 11 }
                            },
                            "cornerB": {
                              "p": { "arcseconds": 12 },
                              "q": { "arcseconds": 13 }
                            }
                          }
                        },
                        "skyOffsets": {
                          "generatorType": "RANDOM",
                          "random": {
                            "size": { "arcseconds":  14 },
                            "center": {
                              "p": { "arcseconds": 15 },
                              "q": { "arcseconds": 16 }
                            }
                          },
                          "spiral": null
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("can create with enumerated generators"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  targetEnvironment: {
                    asterism: [${tid.asJson}]
                  }
                  scienceRequirements: {
                    exposureTimeMode: {
                      signalToNoise: {
                        value: 10.0
                        at: { nanometers: 500.0 }
                      }
                    }
                    imaging: {
                      minimumFov: { arcseconds: 100 }
                      narrowFilters: false
                      broadFilters: false
                      combinedFilters: true
                    }
                  }
                  observingMode: {
                    gmosSouthImaging: {
                      variant: {
                        grouped: {
                          filters: [
                            { filter: G_PRIME },
                            { filter: R_PRIME }
                          ]
                          offsets: {
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
                          skyOffsets: {
                            enumerated: {
                              values: [
                                {
                                  offset: {
                                    p: { arcseconds: 14.0 }
                                    q: { arcseconds: 15.0 }
                                  }
                                  guiding: DISABLED
                                }
                              ]
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            ) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      grouped {
                        offsets {
                          generatorType
                          enumerated {
                            values {
                              offset {
                                p { arcseconds }
                                q { arcseconds }
                              }
                              guiding
                            }
                          }
                        }
                        skyOffsets {
                          generatorType
                          enumerated {
                            values {
                              offset {
                                p { arcseconds }
                                q { arcseconds }
                              }
                              guiding
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          """,
          json"""
            {
              "createObservation": {
                "observation": {
                  "observingMode": {
                    "gmosSouthImaging": {
                      "variant": {
                        "grouped": {
                          "offsets": {
                            "generatorType": "ENUMERATED",
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
                            }
                          },
                          "skyOffsets": {
                            "generatorType": "ENUMERATED",
                            "enumerated": {
                              "values": [
                                {
                                  "offset": {
                                    "p": { "arcseconds": 14 },
                                    "q": { "arcseconds": 15 }
                                  },
                                  "guiding": "DISABLED"
                                }
                              ]
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          """.asRight
        )

  test("can create with spiral generator"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 10.0
                      at: { nanometers: 500.0 }
                    }
                  }
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    variant: {
                      grouped: {
                        filters: [
                          { filter: G_PRIME },
                          { filter: R_PRIME }
                        ]
                        offsets: {
                          spiral: {
                            size: { arcseconds: 14.0 }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      grouped {
                        offsets {
                          generatorType
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
            }
          }
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "variant": {
                      "grouped": {
                        "offsets": {
                          "generatorType": "SPIRAL",
                          "spiral": {
                            "size": { "arcseconds":  14 },
                            "center": {
                              "p": { "arcseconds": 0 },
                              "q": { "arcseconds": 0 }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("can create pre-imaging"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 10.0
                      at: { nanometers: 500.0 }
                    }
                  }
                  imaging: {
                    minimumFov: { arcseconds: 100 }
                    narrowFilters: false
                    broadFilters: false
                    combinedFilters: true
                  }
                }
                observingMode: {
                  gmosSouthImaging: {
                    variant: {
                      preImaging: {
                        filters: [
                          { filter: G_PRIME },
                          { filter: R_PRIME }
                        ],
                        offset1: {
                          p: { arcseconds: 0 }
                          q: { arcseconds: 1 }
                        }
                        offset2: {
                          p: { arcseconds: 2 }
                          q: { arcseconds: 3 }
                        }
                        offset3: {
                          p: { arcseconds: 4 }
                          q: { arcseconds: 5 }
                        }
                        offset4: {
                          p: { arcseconds: 6 }
                          q: { arcseconds: 7 }
                        }
                      }
                    }
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gmosSouthImaging {
                    variant {
                      preImaging {
                        offset1 {
                          p { arcseconds }
                          q { arcseconds }
                        }
                        offset2 {
                          p { arcseconds }
                          q { arcseconds }
                        }
                        offset3 {
                          p { arcseconds }
                          q { arcseconds }
                        }
                        offset4 {
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
        """, json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gmosSouthImaging": {
                    "variant": {
                      "preImaging": {
                        "offset1": {
                          "p": { "arcseconds": 0 },
                          "q": { "arcseconds": 1 }
                        },
                        "offset2": {
                          "p": { "arcseconds": 2 },
                          "q": { "arcseconds": 3 }
                        },
                        "offset3": {
                          "p": { "arcseconds": 4 },
                          "q": { "arcseconds": 5 }
                        },
                        "offset4": {
                          "p": { "arcseconds": 6 },
                          "q": { "arcseconds": 7 }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )