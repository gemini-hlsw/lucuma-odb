// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.CentralWavelength
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan

trait ExecutionTestSupportForIgrins2 extends ExecutionTestSupport:

  def setOffsets(oid: Observation.Id, mode: Igrins2OffsetMode, offsets: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                igrins2LongSlit: {
                  explicitOffsetMode: ${mode.tag.toScreamingSnakeCase}
                  explicitOffsets: $offsets
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

  val Igrins2AtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { seconds }
          centralWavelength { nanometers }
        }
        stepConfig {
          stepType
        }
        telescopeConfig {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
          guiding
        }
        observeClass
        breakpoint
      }
    """

  def igrins2ScienceQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "igrins2", "science", Igrins2AtomQuery, futureLimit)

  protected def igrins2ExpectedScience(exposureTime: TimeSpan, p: BigDecimal, q: BigDecimal, g: StepGuideState): Json =
    val tc = TelescopeConfig(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(p),
        Offset.Q.signedDecimalArcseconds.reverseGet(q)
      ),
      g
    )
    json"""
      {
        "instrumentConfig": {
          "exposure": { "seconds": ${exposureTime.toSeconds} },
          "centralWavelength": { "nanometers": ${CentralWavelength.toNanometers.value.value.asJson} }
        },
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(tc)},
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  protected def igrins2ExpectedScienceAtom(
    exposureTime: TimeSpan,
    offsets: (BigDecimal, BigDecimal, StepGuideState)*
  ): Json =
    val sciSteps = offsets.toList.map((p, q, g) => igrins2ExpectedScience(exposureTime, p, q, g))

    Json.obj(
      "description" -> "ABBA Cycle".asJson,
      "observeClass" -> "SCIENCE".asJson,
      "steps" -> sciSteps.asJson
    )
