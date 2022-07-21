// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
              programId: ${pid.asJson}
              input: {
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
                    fromDecimal: {
                      value: "78.91"
                      units: CENTIMETERS_PER_SECOND
                    }
                  }
                  parallax: {
                    microarcseconds: "123456"
                  }
                }
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: B5_III
                      }
                    }
                    emissionLines: {
                      lines: [
                        {
                          wavelength: {
                            angstroms: "20"
                          }
                          lineWidth: "1.2"
                          lineFlux: {
                            value: "42"
                            units: ERG_PER_S_PER_CM_SQUARED
                          }
                        }
                      ]
                      fluxDensityContinuum: {
                        value: "42"
                        units: W_PER_M_SQUARED_PER_UM
                      }
                    }
                  }
                }
              }
            ) $FullTargetGraph
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "createTarget": {
                "existence": "PRESENT",
                "name": "Crunchy Target",
                "program": {
                  "id": ${pid}
                },
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "sed": {
                        "stellarLibrary": "B5_III",
                        "coolStar": null,
                        "galaxy": null,
                        "planet": null,
                        "quasar": null,
                        "hiiRegion": null,
                        "planetaryNebula": null,
                        "powerLaw": null,
                        "blackBodyTempK": null,
                        "fluxDensities": null
                      }
                    },
                    "emissionLines": {
                      "lines": [
                        {
                          "wavelength": {
                            "angstroms": "20"
                          },
                          "lineWidth": "1.2",
                          "lineFlux": {
                            "value": "42",
                            "units": "ERG_PER_S_PER_CM_SQUARED"
                          }
                        }
                      ],
                      "fluxDensityContinuum": {
                        "value": "42",
                        "units": "W_PER_M_SQUARED_PER_UM"
                      }
                    }
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
                      "microarcsecondsPerYear": 1295999993000,
                      "milliarcsecondsPerYear": 1295999993
                    }
                  },
                  "radialVelocity": {
                    "centimetersPerSecond": 0,
                    "metersPerSecond": 0.7891,
                    "kilometersPerSecond": 0.0007891
                  },
                  "parallax": {
                    "microarcseconds": 123456,
                    "milliarcseconds": 123.456
                  },
                  "catalogInfo": null
                },
                "nonsidereal": null
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
              programId: ${pid.asJson}
              input: {
                name: "Crunchy Planet"
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: B5_III
                      }
                    }
                    emissionLines: {
                      lines: [
                        {
                          wavelength: {
                            angstroms: "20"
                          }
                          lineWidth: "1.2"
                          lineFlux: {
                            value: "42"
                            units: ERG_PER_S_PER_CM_SQUARED
                          }
                        }
                      ]
                      fluxDensityContinuum: {
                        value: "42"
                        units: W_PER_M_SQUARED_PER_UM
                      }
                    }
                  }
                }
                nonsidereal: {
                  des: "foo"
                  keyType: COMET
                }
              }
            ) $FullTargetGraph
          }
        """).flatMap { js =>
          val expected = json"""
            {
              "createTarget" : {
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
                    "emissionLines" : {
                      "lines" : [
                        {
                          "wavelength" : {
                            "angstroms" : "20"
                          },
                          "lineWidth" : "1.2",
                          "lineFlux" : {
                            "value" : "42",
                            "units" : "ERG_PER_S_PER_CM_SQUARED"
                          }
                        }
                      ],
                      "fluxDensityContinuum" : {
                        "value" : "42",
                        "units" : "W_PER_M_SQUARED_PER_UM"
                      }
                    }
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
          """
          IO(assertEquals(js, expected))
        }
    }
  }






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


