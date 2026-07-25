// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.CentralWavelength
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan

trait ExecutionTestSupportForIgrins2 extends ExecutionTestSupport:

  def setOffsets(oid: Observation.Id, mode: SlitOffsetMode, configs: String): IO[Unit] =
    val shape = mode match
      case SlitOffsetMode.NodAlongSlit => "alongSlit"
      case SlitOffsetMode.NodToSky     => "toSky"
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                igrins2LongSlit: {
                  explicitTelescopeConfigs: { $shape: $configs }
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

  /**
   * Turns on the SVC sub-config, optionally overriding the exposure and/or telescope dither
   * positions. Passing no overrides turns SVC on at its defaults.
   */
  def enableIgrins2Svc(
    oid:                       Observation.Id,
    explicitExposureSeconds:  Option[BigDecimal] = None,
    explicitTelescopeConfigs: Option[String] = None
  ): IO[Unit] =
    val fields = List(
      explicitExposureSeconds.map(s => s"explicitExposure: { seconds: $s }"),
      explicitTelescopeConfigs.map(tcs => s"explicitTelescopeConfigs: $tcs")
    ).flatten.mkString(", ")
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                igrins2LongSlit: {
                  svc: { $fields }
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

  private def igrins2ExpectedScience(
    exposureTime: TimeSpan,
    p:            BigDecimal,
    q:            BigDecimal,
    g:            StepGuideState,
    obsClass:     ObserveClass
  ): Json =
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
        "observeClass": ${obsClass.tag.toScreamingSnakeCase.asJson},
        "breakpoint": "DISABLED"
      }
    """

  protected def igrins2ExpectedScienceAtom(
    exposureTime: TimeSpan,
    offsets:      (BigDecimal, BigDecimal, StepGuideState)*
  ): Json =
    igrins2ExpectedScienceAtomAs(ObserveClass.Science, exposureTime, offsets*)

  protected def igrins2ExpectedScienceAtomAs(
    obsClass:     ObserveClass,
    exposureTime: TimeSpan,
    offsets:      (BigDecimal, BigDecimal, StepGuideState)*
  ): Json =
    val sciSteps = offsets.toList.map((p, q, g) => igrins2ExpectedScience(exposureTime, p, q, g, obsClass))

    Json.obj(
      "description"  -> "ABBA Cycle".asJson,
      "observeClass" -> obsClass.tag.toScreamingSnakeCase.asJson,
      "steps"        -> sciSteps.asJson
    )

  def igrins2AcquisitionQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "igrins2", "acquisition", Igrins2AtomQuery, futureLimit)

  private def igrins2ExpectedAcquisitionStep(
    exposureTime: TimeSpan,
    p:            BigDecimal,
    q:            BigDecimal,
    g:            StepGuideState,
    breakpoint:   Breakpoint
  ): Json =
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
        "observeClass": "ACQUISITION",
        "breakpoint": ${breakpoint.tag.toScreamingSnakeCase.asJson}
      }
    """

  /** The single "SVC Acquisition" atom, breakpoint on the final step only. */
  protected def igrins2ExpectedAcquisitionAtom(
    exposureTime: TimeSpan,
    offsets:      (BigDecimal, BigDecimal, StepGuideState)*
  ): Json =
    val lastIndex = offsets.size - 1
    val steps = offsets.toList.zipWithIndex.map { case ((p, q, g), ix) =>
      igrins2ExpectedAcquisitionStep(
        exposureTime, p, q, g,
        if ix == lastIndex then Breakpoint.Enabled else Breakpoint.Disabled
      )
    }

    Json.obj(
      "description"  -> "SVC Acquisition".asJson,
      "observeClass" -> "ACQUISITION".asJson,
      "steps"        -> steps.asJson
    )
