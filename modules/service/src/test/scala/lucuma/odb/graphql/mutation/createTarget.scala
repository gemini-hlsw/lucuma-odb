// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Partner
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class createTarget extends OdbSuite with CreateProgramOps with LinkUserOps with SetAllocationOps {
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

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs) // TODO: something cheaper

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
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "createTarget": {
                "target": {
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
              }
            }
          """
          IO(assertEquals(js, expected))
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
            }
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "createTarget" : {
                "target" : {
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
              }
            }
          """
          IO(assertEquals(js, expected))
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