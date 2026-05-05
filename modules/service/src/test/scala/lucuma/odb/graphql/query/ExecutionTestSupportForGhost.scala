// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.odb.sequence.ghost.CentralWavelength


trait ExecutionTestSupportForGhost extends ExecutionTestSupport:

  val GhostAtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
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

  def detector(d: GhostDetector): Json =
    json"""
      {
        "exposureTime": {
          "seconds": ${d.exposureTime.toSeconds.asJson}
        },
        "exposureCount": ${d.exposureCount.value.asJson},
        "binning": ${d.binning.asJson},
        "readMode": ${d.readMode.asJson}
      }
    """

  def expectedInstrumentConfig(c: GhostDynamicConfig): Json =
    json"""
      {
        "red": ${detector(c.red.value)},
        "blue": ${detector(c.blue.value)},
        "ifu1FiberAgitator": ${c.ifu1FiberAgitator.asJson},
        "ifu2FiberAgitator": ${c.ifu2FiberAgitator.asJson},
        "centralWavelength": {
          "nanometers": ${CentralWavelength.toNanometers.value.value.asJson}
        }
      }
    """

  def expectedStep(c: GhostDynamicConfig): Json =
    json"""
      {
        "instrumentConfig": ${expectedInstrumentConfig(c)},
        "stepConfig": {
          "stepType": "SCIENCE"
        },
        "telescopeConfig": {
          "offset": {
            "p": { "arcseconds": 0.000000 },
            "q": { "arcseconds": 0.000000 }
          },
          "guiding": "ENABLED"
        },
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  def expectedAtom(c: GhostDynamicConfig): Json =
    json"""
      {
        "description": null,
        "observeClass": "SCIENCE",
        "steps": [
          ${expectedStep(c)}
        ]
      }
    """

  def scienceQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    s"""
      query {
        executionConfig(observationId: "$oid"${futureLimit.fold("")(lim => s", futureLimit: $lim")}) {
          ghost {
            static {
              resolutionMode
              slitViewingCameraExposureTime { seconds }
            }
            science {
              nextAtom {
                $GhostAtomQuery
              }
              possibleFuture {
                $GhostAtomQuery
              }
              hasMore
            }
          }
        }
      }
    """