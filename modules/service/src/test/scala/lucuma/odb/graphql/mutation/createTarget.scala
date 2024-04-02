// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Partner
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.TargetRole

class createTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

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
                      "fluxDensities" : null
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
                  "microarcseconds": 44442000000
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
              "nonsidereal": null
            }
          """

          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getTargetRoleFromDb(id).map(role => assertEquals(role, TargetRole.Science))
        }
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
                      "fluxDensities" : null
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
              }
            }
          """
          
          val data = js.hcursor.downFields("createTarget", "target").as[Json].toOption.get
          assertEquals(data, expected)

          // The create target mutation only creates science targets.
          val id = js.hcursor.downFields("createTarget", "targetId", "id").as[Target.Id].toOption.get
          getTargetRoleFromDb(id).map(role => assertEquals(role, TargetRole.Science))
        }
    }
  }

  test("[general] can create a target with a proposal reference") {
    createProgramAs(pi).flatMap { pid =>
      addProposal(pi, pid) *>
      setSemester(pi, pid, Semester.unsafeFromString("2025A")) *>
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
            microarcseconds
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
      }
     """

}
