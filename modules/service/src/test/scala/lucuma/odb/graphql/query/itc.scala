// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.traverse.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan

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

  def createObservation(pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
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
    ).map(
      _.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
    )

  def setup(targetCount: Int = 1): IO[(Program.Id, Observation.Id, List[Target.Id])] =
    for {
      p  <- createProgram
      ts <- (1 to targetCount).toList.traverse(_ => createTarget(p))
      o  <- createObservation(p, ts)
    } yield (p, o, ts)

  def setup1: IO[(Program.Id, Observation.Id, Target.Id)] =
    setup(1).map {
      case (pid, oid, List(tid)) => (pid, oid, tid)
      case _                     => sys.error("Expected a single target")
    }

  def setup2: IO[(Program.Id, Observation.Id, Target.Id, Target.Id)] =
    setup(2).map {
      case (pid, oid, List(tid0, tid1)) => (pid, oid, tid0, tid1)
      case _                            => sys.error("Expected two targets")
    }

  test("success, one target") {
    setup1.flatMap { case (pid, oid, tid) =>
      expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$pid", observationId: "$oid") {
                programId
                observationId
                result {
                  __typename
                  status
                  ... on ItcSuccess {
                    targetId
                    exposureTime {
                      seconds
                    }
                    exposures
                    signalToNoise
                  }
                }
                all {
                  status
                  ... on ItcSuccess {
                    targetId
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "programId": $pid,
                 "observationId": $oid,
                 "result": {
                   "__typename": "ItcSuccess",
                   "status": "SUCCESS",
                   "targetId": $tid,
                   "exposureTime": {
                     "seconds": 10.000000
                   },
                   "exposures": ${FakeItcResult.exposures.value},
                   "signalToNoise": ${FakeItcResult.signalToNoise.value}
                 },
                 "all": [
                   {
                     "status": "SUCCESS",
                     "targetId": $tid
                   }
                 ]
               }
            }
          """
        )
      )
    }
  }

  test("success, two targets") {
    setup2.flatMap { case (pid, oid, tid0, tid1) =>
      expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$pid", observationId: "$oid") {
                result {
                  ... on ItcSuccess {
                    targetId
                  }
                }
                all {
                  ... on ItcSuccess {
                    targetId
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "result": {
                   "targetId": $tid0
                 },
                 "all": [
                   {
                     "targetId": $tid0
                   },
                   {
                     "targetId": $tid1
                   }
                 ]
               }
            }
          """
        )
      )
    }
  }

  test("observation missing observingMode") {
    def createObservation(pid: Program.Id, tid: Target.Id): IO[Observation.Id] =
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
                   asterism: [ ${tid.asJson} ]
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

    for {
      p <- createProgram
      t <- createTarget(p)
      o <- createObservation(p, t)
      r <- expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$p", observationId: "$o") {
                result {
                  status
                  ... on ItcMissingParams {
                    targetId
                    params
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "result": {
                   "status": "MISSING_PARAMS",
                   "targetId": null,
                   "params": [
                     "observing mode"
                   ]
                 }
               }
            }
          """
        )
      )
    } yield r
  }

  test("observation missing targets") {
    def createObservation(pid: Program.Id): IO[Observation.Id] =
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

    for {
      p <- createProgram
      o <- createObservation(p)
      r <- expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$p", observationId: "$o") {
                result {
                  status
                  ... on ItcMissingParams {
                    targetId
                    params
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "result": {
                   "status": "MISSING_PARAMS",
                   "targetId": null,
                   "params": [
                     "target"
                   ]
                 }
               }
            }
          """
        )
      )
    } yield r
  }

  test("target missing rv and brightness") {

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
      ).map(
        _.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
      )

    for {
      p <- createProgram
      t <- createTarget(p)
      o <- createObservation(p, List(t))
      r <- expect(
        user = user,
        query =
          s"""
            query {
              itc(programId: "$p", observationId: "$o") {
                result {
                  status
                  ... on ItcMissingParams {
                    targetId
                    params
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
               "itc": {
                 "result": {
                   "status": "MISSING_PARAMS",
                   "targetId": $t,
                   "params": [
                     "brightness measure",
                     "radial velocity"
                   ]
                 }
               }
            }
          """
        )
      )
    } yield r
  }
}
