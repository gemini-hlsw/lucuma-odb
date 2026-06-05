// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

class executionPlannedTime_AlienVisitor extends ExecutionTestSupport:

  // Alien visitor program requested time
  private val TotalRequestTime: TimeSpan = 42.minTimeSpan

  private def setTotalRequestTime(oid: Observation.Id, mode: VisitorObservingModeType, time: TimeSpan): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                visitor: {
                  mode: ${mode.tag.toScreamingSnakeCase}
                  centralWavelength: { nanometers: 2200 }
                  agsDiameter: { arcseconds: 1 }
                  name: "test visitor"
                  totalRequestTime: { milliseconds: ${time.toMilliseconds} }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def digestQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            digest {
              calculationState
              value {
                setup {
                  full { seconds }
                  reacquisition { seconds }
                }
                setupCount
                science {
                  observeClass
                  timeEstimate {
                    program { seconds }
                    nonCharged { seconds }
                    total { seconds }
                  }
                  atomCount
                }
              }
            }
          }
        }
      }
    """

  private def expectedDigest(time: TimeSpan): Json =
    val programTime = time.toSeconds.setScale(6)
    json"""
      {
        "observation": {
          "execution": {
            "digest": {
              "calculationState": "READY",
              "value": {
                "setup": {
                  "full":          { "seconds": 0.000000 },
                  "reacquisition": { "seconds": 0.000000 }
                },
                "setupCount": 0,
                "science": {
                  "observeClass": "SCIENCE",
                  "timeEstimate": {
                    "program":    { "seconds": ${programTime.asJson} },
                    "nonCharged": { "seconds": 0.000000 },
                    "total":      { "seconds": ${programTime.asJson} }
                  },
                  "atomCount": 0
                }
              }
            }
          }
        }
      }
    """

  private val testCases: List[VisitorObservingModeType] =
    List(VisitorObservingModeType.VisitorNorth, VisitorObservingModeType.VisitorSouth)

  testCases.foreach: mode =>
    test(s"[planned time]: ${mode.tag}"):
      val setup: IO[Observation.Id] =
        for
          p <- createProgram
          t <- createTargetWithProfileAs(pi, p)
          o <- createVisitorModeObservationAs(pi, p, mode, t)
          _ <- setTotalRequestTime(o, mode, TotalRequestTime)
          _ <- runObscalcUpdate(p, o)
        yield o

      setup.flatMap: oid =>
        expect(pi, digestQuery(oid), expectedDigest(TotalRequestTime).asRight)
