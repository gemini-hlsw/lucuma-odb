// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.gnirsIfuTelescopeConfigPresets
import lucuma.core.syntax.string.*
import lucuma.odb.graphql.feature.TelluricCalibrationsTestSupport
import lucuma.odb.graphql.query.ExecutionTestSupportForGnirs
import lucuma.odb.json.stepconfig.given

import java.time.Instant

// sc-9993: an HR-IFU telluric standard observes with the "Telluric" offset preset, not the
// science's offsets.
class ShortCut_9993
  extends OdbSuite
  with ExecutionTestSupportForGnirs
  with TelluricCalibrationsTestSupport:

  val when: Instant = Instant.parse("2024-01-01T12:00:00Z")

  private val HighResolutionPresets: Map[String, NonEmptyList[TelescopeConfig]] =
    gnirsIfuTelescopeConfigPresets(GnirsFpuIfu.HighResolution).toList.toMap

  // The HR IFU goes with the long camera.
  private def setHighResolutionIfu(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsSpectroscopy: {
                  camera: LONG_BLUE
                  ifu: { fpu: ${GnirsFpuIfu.HighResolution.tag.toScreamingSnakeCase} }
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

  private def ifuTelescopeConfigs(oid: Observation.Id): IO[List[TelescopeConfig]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              gnirsSpectroscopy {
                ifu {
                  telescopeConfigs {
                    offset { p { microarcseconds } q { microarcseconds } }
                    guiding
                  }
                }
              }
            }
          }
        }
      """
    ).map:
      _.hcursor
        .downFields("observation", "observingMode", "gnirsSpectroscopy", "ifu", "telescopeConfigs")
        .require[List[TelescopeConfig]]

  test("an HR-IFU telluric takes the Telluric offset preset"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGnirsIfuObservationAs(pi, pid, tid)
      _   <- setHighResolutionIfu(oid)
      _   <- runObscalcUpdate(pid, oid)
      _   <- recalculateCalibrations(pid, when, oid)
      tel <- calibrationObservationFor(oid, CalibrationRole.Telluric).map(_.get)
      tcs <- ifuTelescopeConfigs(tel)
    yield assertEquals(tcs, HighResolutionPresets("Telluric").toList)
