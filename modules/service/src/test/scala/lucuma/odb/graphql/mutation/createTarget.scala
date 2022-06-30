package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Partner
import lucuma.core.model.Program
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

  test("[general] create a target") {
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
            )
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

        }
        """).flatMap { js =>

        println(js)

        val get = js.hcursor
          .downField("createTarget")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

}