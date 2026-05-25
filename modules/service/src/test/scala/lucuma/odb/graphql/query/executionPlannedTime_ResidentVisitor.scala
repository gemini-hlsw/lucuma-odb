// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*

class executionPlannedTime_ResidentVisitor extends ExecutionTestSupport:

  private val ScienceEtm: TimeAndCountMode =
    TimeAndCountMode(30.secTimeSpan, PosInt.unsafeFrom(4), Wavelength.unsafeFromIntPicometers(700_000))

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

  private def expectedDigest(setupTs: BigDecimal, readoutTs: BigDecimal): Json =
    val exposure = ScienceEtm.time.toSeconds
    val count    = BigDecimal(ScienceEtm.count.value)
    val program  = ((exposure + readoutTs) * count).setScale(6)
    val setup    = setupTs.setScale(6)

    json"""
      {
        "observation": {
          "execution": {
            "digest": {
              "calculationState": "READY",
              "value": {
                "setup": {
                  "full":          { "seconds": ${setup.asJson} },
                  "reacquisition": { "seconds": 0.000000 }
                },
                "setupCount": 1,
                "science": {
                  "observeClass": "SCIENCE",
                  "timeEstimate": {
                    "program":    { "seconds": ${program.asJson} },
                    "nonCharged": { "seconds": 0.000000 },
                    "total":      { "seconds": ${program.asJson} }
                  },
                  "atomCount": 0
                }
              }
            }
          }
        }
      }
    """

  // setup (seconds), readout (seconds) — must match V1149__visitor_planned_time.sql.
  private val testCases: List[(VisitorObservingModeType, BigDecimal, BigDecimal)] =
    List(
      (VisitorObservingModeType.AlopekeSpeckle,   BigDecimal(300), BigDecimal("0.004")),
      (VisitorObservingModeType.AlopekeWideField, BigDecimal(360), BigDecimal("0.005")),
      (VisitorObservingModeType.ZorroSpeckle,     BigDecimal(300), BigDecimal("0.004")),
      (VisitorObservingModeType.ZorroWideField,   BigDecimal(360), BigDecimal("0.005")),
      (VisitorObservingModeType.MaroonX,          BigDecimal(300), BigDecimal(100))
    )

  testCases.foreach: (mode, setupTs, readoutTs) =>
    test(s"[planned time]: ${mode.tag}"):
      val setup: IO[Observation.Id] =
        for
          p <- createProgram
          t <- createTargetWithProfileAs(pi, p)
          o <- createVisitorModeObservationAs(pi, p, mode, t)
          _ <- runObscalcUpdate(p, o)
        yield o

      setup.flatMap: oid =>
        expect(pi, digestQuery(oid), expectedDigest(setupTs, readoutTs).asRight)
