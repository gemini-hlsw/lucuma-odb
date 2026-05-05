// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.syntax.timespan.*

class executionSciGhostIfu extends ExecutionTestSupportForGhost:

  val StepCount: Int = 3

  val mode = s"""
    ghostIfu: {
      stepCount: $StepCount
      resolutionMode: STANDARD
      red: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 10.0 }
            count: 2
            at: { nanometers: 500 }
          }
        }
        explicitBinning: ONE_BY_TWO
      }
      blue: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 30.0 }
            count: 4
            at: { nanometers: 500 }
          }
        }
        explicitReadMode: FAST
      }
      slitViewingCameraExposureTime: { seconds: 5.0 }
      explicitIfu1Agitator: ENABLED
    }
  """

  val config = GhostDynamicConfig(
    GhostDetector(
      10.secondTimeSpan,
      PosInt.unsafeFrom(2),
      GhostBinning.OneByTwo,
      GhostReadMode.DefaultRed
    ).asRed,
    GhostDetector(
      30.secondTimeSpan,
      PosInt.unsafeFrom(4),
      GhostBinning.OneByOne,
      GhostReadMode.Fast
    ).asBlue,
    GhostIfu1FiberAgitator.Enabled,
    GhostIfu2FiberAgitator.Disabled
  )

  val expected: Json =
    Json.obj(
      "executionConfig" -> Json.obj(
        "ghost" -> Json.obj(
          "static" -> Json.obj(
            "resolutionMode" -> GhostResolutionMode.Standard.asJson,
            "slitViewingCameraExposureTime" -> Json.obj(
              "seconds" -> BigDecimal("5.000000").asJson
            )
          ),
          "science" -> Json.obj(
            "nextAtom" -> expectedAtom(config),
            "possibleFuture" -> List.fill(StepCount-1)(expectedAtom(config)).asJson,
            "hasMore" -> Json.fromBoolean(false)
          )
        )
      )
    )

  test("generation - pre visit"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = scienceQuery(oid),
        expected = expected.asRight
      )

  test("generation - post visit"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
        _ <- recordVisitAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = scienceQuery(oid),
        expected = expected.asRight
      )

  test("execution"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
      v <- recordVisitAs(serviceUser, o)
      s <- firstScienceStepId(serviceUser, o)
      _ <- addEndStepEvent(s, v)
      _ <- expect(
            pi,
            s"""
              query {
                observation(observationId: "$o") {
                  execution {
                    atomRecords {
                      matches {
                        steps {
                          matches {
                            index
                            ghost {
                              red {
                                exposureTime { seconds }
                                exposureCount
                                binning
                                readMode
                              }
                              blue {
                                exposureTime { seconds }
                                exposureCount
                                binning
                                readMode
                              }
                              ifu1FiberAgitator
                              ifu2FiberAgitator
                              centralWavelength { nanometers }
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
                "observation": {
                  "execution": {
                    "atomRecords": {
                      "matches": [
                        {
                          "steps": {
                            "matches": [
                              {
                                "index": 1,
                                "ghost": ${expectedInstrumentConfig(config)}
                              }
                            ]
                          }
                        }
                      ]
                    }
                  }
                }
              }
            """.asRight
          )
    yield ()