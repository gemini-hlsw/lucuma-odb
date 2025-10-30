// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class executionFailures extends ExecutionTestSupportForGmos {

  test("ITC failure") {
    // Creates an observation with a central wavelength of 666, which prompts
    // the test ITC to produce an error instead of a result. (See OdbSuite
    // itcClient.)
    def createObservation(
      user: User,
      pid:  Program.Id,
      tids: List[Target.Id]
    ): IO[Observation.Id] =
      createObservationWithModeAs(
        user,
        pid,
        tids,
        """
          gmosNorthLongSlit: {
            grating: R831_G5302,
            fpu: LONG_SLIT_0_50,
            centralWavelength: { nanometers: 666 }
          }
        """
      )

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expectOdbError(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps { instrumentConfig { exposure { seconds } } }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
          expected = {
            case OdbError.RemoteServiceCallError(Some("Error calling ITC service: Artifical exception for test cases.")) => // ok
          }
      )
    }

  }

  test("cannot generate, missing mode") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield o

    setup.flatMap { oid =>
      expectOdbError(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       static {
                         stageMode
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = {
          case OdbError.SequenceUnavailable(oid, Some("Could not generate a sequence for o-101: observation is missing observing mode")) => //ok
        }
      )
    }
  }

  test("simple generation - too many future atoms") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config(futureLimit: 101) {
                     gmosNorth {
                       science {
                         possibleFuture {
                           observeClass
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = List("Argument 'futureLimit' is invalid: Future limit must range from 0 to 100, but was 101.").asLeft
      )
    }
  }

  test("user cannot access program") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi2,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     instrument
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "observation": null
            }
          """
        )
      )
    }

  }

  test("cross site execution config") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosSouth {
                       science {
                         nextAtom {
                           steps {
                             instrumentConfig {
                               gratingConfig {
                                 wavelength { nanometers }
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
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosSouth": null
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("unimplemented calibration role") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.Photometric)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           observeClass
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = List(s"Could not generate a sequence for $oid: GMOS Long Slit photometric not implemented").asLeft
      )
    }
  }
}
