// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CallForProposalsType.DemoScience
import lucuma.core.enums.Partner
import lucuma.core.model.ProgramReference
import lucuma.core.model.Semester
import lucuma.core.model.Target

class createTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  // Unsafe convenience for testing.
  extension (s: String) {
    def programReference: ProgramReference =
      ProgramReference.fromString.unsafeGet(s)
  }

  val specPhoto  = "SYS-SPECTROPHOTOMETRIC".programReference

  test("[general] create a sidereal target") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Target"
                  sidereal: {
                    ra: {
                      degrees: "12.345"
                    }
                    dec: {
                      degrees: "45.678"
                    }
                    epoch: "J2000.000"
                    properMotion: {
                      ra: {
                        milliarcsecondsPerYear: "12.345"
                      }
                      dec: {
                        milliarcsecondsPerYear: "-7.0"
                      }
                    }
                    radialVelocity: {
                      centimetersPerSecond: "78"
                    }
                    parallax: {
                      microarcseconds: "123456"
                    }
                    catalogInfo: {
                      name: SIMBAD
                      id: "arbitrary"
                      objectType: "also arbitrary"
                    }
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
                }
              }
            ) {
              target $FullTargetGraph
              targetId: target { id }
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "existence": "PRESENT",
              "name": "Crunchy Target",
              "program": {
                "id": ${pid}
              },
              "sourceProfile": {
                "point" : {
                  "bandNormalized" : {
                    "sed" : {
                      "stellarLibrary" : "B5_III",
                      "coolStar" : null,
                      "galaxy" : null,
                      "planet" : null,
                      "quasar" : null,
                      "hiiRegion" : null,
                      "planetaryNebula" : null,
                      "powerLaw" : null,
                      "blackBodyTempK" : null,
                      "fluxDensities" : null,
                      "fluxDensitiesAttachment" : null
                    }
                  },
                  "emissionLines" : null
                }
              },
              "sidereal": {
                "ra": {
                  "hms": "00:49:22.800000",
                  "hours": 0.823,
                  "degrees": 12.345,
                  "microseconds": 2962800000
                },
                "dec": {
                  "dms": "+45:40:40.800000",
                  "degrees": 45.678,
                  "microarcseconds": 164440800000
                },
                "epoch": "J2000.000",
                "properMotion": {
                  "ra": {
                    "microarcsecondsPerYear": 12345,
                    "milliarcsecondsPerYear": 12.345
                  },
                  "dec": {
                    "microarcsecondsPerYear": -7000,
                    "milliarcsecondsPerYear": -7.000
                  }
                },
                "radialVelocity": {
                  "centimetersPerSecond" : 78,
                  "metersPerSecond" : 0.78,
                  "kilometersPerSecond" : 0.00078
                },
                "parallax": {
                  "microarcseconds": 123456,
                  "milliarcseconds": 123.456
                },
                "catalogInfo": {
                  "name" : "SIMBAD",
                  "id" : "arbitrary",
                  "objectType" : "also arbitrary"
                }
              },
              "nonsidereal": null,
              "opportunity": null
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getCalibrationRoleFromDb(id).map(role => assert(role.isEmpty))
        }
    }
  }

  test("[general] create a sidereal target with defaults for pm/rv/parallax") {
    createProgramAs(pi).flatMap { pid =>
      expect(pi,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Target with Defaults"
                  sidereal: {
                    ra: {
                      degrees: "10.0"
                    }
                    dec: {
                      degrees: "20.0"
                    }
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
                }
              }
            ) {
              target {
                existence
                name
                program {
                  id
                }
                sidereal {
                  ra {
                    degrees
                  }
                  dec {
                    degrees
                  }
                  epoch
                  properMotion {
                    ra {
                      milliarcsecondsPerYear
                    }
                    dec {
                      milliarcsecondsPerYear
                    }
                  }
                  radialVelocity {
                    metersPerSecond
                  }
                  parallax {
                    microarcseconds
                  }
                }
              }
            }
          }
        """,
        json""" {
          "createTarget": {
            "target": {
              "existence": "PRESENT",
              "name": "Target with Defaults",
              "program": {
                "id": $pid
              },
              "sidereal": {
                "ra": {
                  "degrees": 10.0
                },
                "dec": {
                  "degrees": 20.0
                },
                "epoch": "J2000.000",
                "properMotion": {
                  "ra": {
                    "milliarcsecondsPerYear": 0.000
                  },
                  "dec": {
                    "milliarcsecondsPerYear": 0.000
                  }
                },
                "radialVelocity": {
                  "metersPerSecond": 0.000
                },
                "parallax": {
                  "microarcseconds": 0
                }
              }
            }
          }
        }
      """.asRight)
    }
  }

  test("[general] create a nonsidereal target") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Planet"
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
                  nonsidereal: {
                    des: "foo"
                    keyType: COMET
                  }
                }
              }
            ) {
              target $FullTargetGraph
              targetId: target { id }
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "existence" : "PRESENT",
              "name" : "Crunchy Planet",
              "program" : {
                "id" : $pid
              },
              "sourceProfile" : {
                "point" : {
                  "bandNormalized" : {
                    "sed" : {
                      "stellarLibrary" : "B5_III",
                      "coolStar" : null,
                      "galaxy" : null,
                      "planet" : null,
                      "quasar" : null,
                      "hiiRegion" : null,
                      "planetaryNebula" : null,
                      "powerLaw" : null,
                      "blackBodyTempK" : null,
                      "fluxDensities" : null,
                      "fluxDensitiesAttachment" : null
                    }
                  },
                  "emissionLines" : null
                }
              },
              "sidereal" : null,
              "nonsidereal" : {
                "des" : "foo",
                "keyType" : "COMET",
                "key" : "Comet_foo"
              },
              "opportunity": null
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getCalibrationRoleFromDb(id).map(role => assert(role.isEmpty))
        }
    }
  }

  test("[general] create a target of opportunity") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Planet"
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
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: PARTIAL
                        start: { degrees: "10.000" }
                        end: { degrees: "20.000" }
                      }
                      declinationArc: {
                        type: PARTIAL
                        start: { degrees: "40.000" }
                        end: { degrees: "50.000" }
                      }
                    }
                  }
                }
              }
            ) {
              target $FullTargetGraph
              targetId: target { id }
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "existence" : "PRESENT",
              "name" : "Crunchy Planet",
              "program" : {
                "id" : $pid
              },
              "sourceProfile" : {
                "point" : {
                  "bandNormalized" : {
                    "sed" : {
                      "stellarLibrary" : "B5_III",
                      "coolStar" : null,
                      "galaxy" : null,
                      "planet" : null,
                      "quasar" : null,
                      "hiiRegion" : null,
                      "planetaryNebula" : null,
                      "powerLaw" : null,
                      "blackBodyTempK" : null,
                      "fluxDensities" : null,
                      "fluxDensitiesAttachment" : null
                    }
                  },
                  "emissionLines" : null
                }
              },
              "sidereal" : null,
              "nonsidereal" : null,
              "opportunity" : {
                "region" : {
                  "rightAscensionArc" : {
                    "type" : "PARTIAL",
                    "start" : {
                      "hms" : "00:40:00.000000",
                      "hours" : 0.6666666666666666,
                      "degrees" : 10.0,
                      "microseconds" : 2400000000
                    },
                    "end" : {
                      "hms" : "01:20:00.000000",
                      "hours" : 1.3333333333333333,
                      "degrees" : 20.0,
                      "microseconds" : 4800000000
                    }
                  },
                  "declinationArc" : {
                    "type" : "PARTIAL",
                    "start" : {
                      "dms" : "+40:00:00.000000",
                      "degrees" : 40.0,
                      "microarcseconds" : 144000000000
                    },
                    "end" : {
                      "dms" : "+50:00:00.000000",
                      "degrees" : 50.0,
                      "microarcseconds" : 180000000000
                    }
                  }
                }
              }
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getCalibrationRoleFromDb(id).map(role => assert(role.isEmpty))
        }
    }
  }

  test("[general] create a target of opportunity (degenerate cases)") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Planet"
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
                  opportunity: {
                    region: {
                      rightAscensionArc: {
                        type: FULL
                      }
                      declinationArc: {
                        type: EMPTY
                      }
                    }
                  }
                }
              }
            ) {
              target $FullTargetGraph
              targetId: target { id }
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "existence" : "PRESENT",
              "name" : "Crunchy Planet",
              "program" : {
                "id" : $pid
              },
              "sourceProfile" : {
                "point" : {
                  "bandNormalized" : {
                    "sed" : {
                      "stellarLibrary" : "B5_III",
                      "coolStar" : null,
                      "galaxy" : null,
                      "planet" : null,
                      "quasar" : null,
                      "hiiRegion" : null,
                      "planetaryNebula" : null,
                      "powerLaw" : null,
                      "blackBodyTempK" : null,
                      "fluxDensities" : null,
                      "fluxDensitiesAttachment" : null
                    }
                  },
                  "emissionLines" : null
                }
              },
              "sidereal" : null,
              "nonsidereal" : null,
              "opportunity" : {
                "region" : {
                  "rightAscensionArc" : {
                    "type" : "FULL",
                    "start" : null,
                    "end" : null
                  },
                  "declinationArc" : {
                    "type" : "EMPTY",
                    "start" : null,
                    "end" : null
                  }
                }
              }
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getCalibrationRoleFromDb(id).map(role => assert(role.isEmpty))
        }
    }
  }


  test("[general] can create a target with a proposal reference") {
    createProgramWithUsPi(pi).flatMap { pid =>
      createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A")).flatMap { cid =>
        addDemoScienceProposal(pi, pid, cid)
      } *>
      submitProposal(pi, pid) *>
      query(pi,
        s"""
          mutation {
            createTarget(
              input: {
                proposalReference: "G-2025A-0001"
                SET: {
                  name: "Crunchy Planet"
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
                  nonsidereal: {
                    des: "foo"
                    keyType: COMET
                  }
                }
              }
            ) {
              targetId: target { id }
            }
          }
        """
      ).map { js =>
        assert(js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.isDefined)
      }
    }
  }

  test("[calibration target] generate a calibration target by putting it in a program with a calibration role") {
    fetchPid(staff, specPhoto).flatMap { pid =>
      query(staff,
        s"""
          mutation {
            createTarget(
              input: {
                programId: ${pid.asJson}
                SET: {
                  name: "Crunchy Target"
                  sidereal: {
                    ra: {
                      degrees: "12.345"
                    }
                    dec: {
                      degrees: "45.678"
                    }
                    epoch: "J2000.000"
                    properMotion: {
                      ra: {
                        milliarcsecondsPerYear: "12.345"
                      }
                      dec: {
                        milliarcsecondsPerYear: "-7.0"
                      }
                    }
                    radialVelocity: {
                      centimetersPerSecond: "78"
                    }
                    parallax: {
                      microarcseconds: "123456"
                    }
                    catalogInfo: {
                      name: SIMBAD
                      id: "arbitrary"
                      objectType: "also arbitrary"
                    }
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
                }
              }
            ) {
              target {
                calibrationRole
              }
              targetId: target { id }
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "calibrationRole": "SPECTROPHOTOMETRIC"
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getCalibrationRoleFromDb(id).map(role => assertEquals(role, CalibrationRole.SpectroPhotometric.some))
        }
    }
  }

}


object createTarget {

  /** The entire target model, as a return value, excluding target ID. */
  val FullTargetGraph =
    s"""
      {
        existence
        name
        program {
          id
        }
        sourceProfile {
          point {
            bandNormalized {
              sed {
                stellarLibrary
                coolStar
                galaxy
                planet
                quasar
                hiiRegion
                planetaryNebula
                powerLaw
                blackBodyTempK
                fluxDensities {
                  wavelength {
                    picometers
                    angstroms
                    nanometers
                    micrometers
                  }
                  density
                }
                fluxDensitiesAttachment
              }
            }
            emissionLines {
              lines {
                wavelength {
                  # picometers
                  angstroms
                  # nanometers
                  # micrometers
                }
                lineWidth
                lineFlux {
                  value
                  units
                }
              }
              fluxDensityContinuum {
                value
                units
              }
            }
          }
        }
        sidereal {
          ra {
            hms
            hours
            degrees
            microseconds
          }
          dec {
            dms
            degrees
            microarcseconds
          }
          epoch
          properMotion {
            ra {
              microarcsecondsPerYear
              milliarcsecondsPerYear
            }
            dec {
              microarcsecondsPerYear
              milliarcsecondsPerYear
            }
          }
          radialVelocity {
            centimetersPerSecond
            metersPerSecond
            kilometersPerSecond
          }
          parallax {
            microarcseconds
            milliarcseconds
          }
          catalogInfo {
            name
            id
            objectType
          }
        }
        nonsidereal {
          des
          keyType
          key
        }
        opportunity {
          region {
            rightAscensionArc {
              type
              start {
                hms
                hours
                degrees
                microseconds
              }
              end {
                hms
                hours
                degrees
                microseconds
              }
            }
            declinationArc {
              type
              start {
                dms
                degrees
                microarcseconds
              }
              end {
                dms
                degrees
                microarcseconds
              }
            }
          }
        }
      }
     """

}
