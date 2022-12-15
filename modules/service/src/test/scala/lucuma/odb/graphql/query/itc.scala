// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class itc extends OdbSuite {

  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(user)

  val createProgram: IO[Program.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createProgram(input: { SET: { name: "ITC Testing" } }) {
             program {
               id
             }
           }
         }
      """
    ).map { json =>
      json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
    }

  def createObservation(pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createObservation(input: {
             programId: ${pid.asJson},
             SET: {
               constraintSet: {
                 cloudExtinction: POINT_ONE,
                 imageQuality: POINT_ONE,
                 skyBackground: DARKEST
               },
               targetEnvironment: {
                 asterism: ${tids.map(tid => s"\"${tid.toString}\"").mkString("[", ", ", "]")}
               },
               scienceRequirements: {
                 mode: SPECTROSCOPY,
                 spectroscopy: {
                   wavelength: {
                     nanometers: 500
                   },
                   resolution: 100,
                   signalToNoise: 100.0,
                   wavelengthCoverage: {
                     nanometers: 20
                   },
                   focalPlane: SINGLE_SLIT,
                   focalPlaneAngle: {
                     microarcseconds: 0
                   }
                 }
               },
               observingMode: {
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
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
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createTarget(pid: Program.Id): IO[Target.Id] =
    query(
      user  = user,
      query =
      s"""
         mutation {
           createTarget(input: {
             programId: ${pid.asJson},
             SET: {
               name: "V1647 Orionis"
               sidereal: {
                 ra: { hms: "05:46:13.137" },
                 dec: { dms: "-00:06:04.89" },
                 epoch: "J2000.0",
                 properMotion: {
                   ra: {
                     milliarcsecondsPerYear: 0.918
                   },
                   dec: {
                     milliarcsecondsPerYear: -1.057
                   },
                 },
                 radialVelocity: {
                   kilometersPerSecond: 27.58
                 },
                 parallax: {
                   milliarcseconds: 2.422
                 }
               },
               sourceProfile: {
                 point: {
                   bandNormalized: {
                     sed: {
                       stellarLibrary: O5_V
                     },
                     brightnesses: [
                       {
                         band: J,
                         value: 14.74,
                         units: VEGA_MAGNITUDE
                       },
                       {
                         band: V,
                         value: 18.1,
                         units: VEGA_MAGNITUDE
                       }
                     ]
                   }
                 }
               }
             }
           }) {
             target {
               id
             }
           }
         }
      """
    ).map { json =>
      json.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
    }

  val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
    for {
      p <- createProgram
      t <- createTarget(p)
      o <- createObservation(p, t)
    } yield (p, o, t)

  test("simple itc") {
    setup.flatMap { case (pid, oid, _) =>
      expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$pid", observationId: "$oid") {
                exposureTime {
                  seconds
                }
                exposures
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "exposureTime": {
                   "seconds": ${FakeItcResult.exposureTime.value.getSeconds}
                 },
                 "exposures": ${FakeItcResult.exposures.value}
               }
            }
          """
        )
      )
    }
  }

}
