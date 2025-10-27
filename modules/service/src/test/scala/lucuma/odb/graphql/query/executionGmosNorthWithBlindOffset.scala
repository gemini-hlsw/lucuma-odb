// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.RadialVelocity
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput

/**
  * These two tests execute a gmos sequence one with a blind offset the other without
  * But witteh same target and mode.
  * The acq for the one with blind offset should use the itc result for the blind offset target
  */
class executionGmosNorthWithBlindOffset extends ExecutionTestSupportForGmos:

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(
      20.secTimeSpan,
      PosInt.unsafeFrom(6),
    )

  // Override static acquisition configs to match flat timing from runtime ITC mock
  override def gmosNorthAcq1(roi: GmosLongSlitAcquisitionRoi): GmosNorth =
    gmosNorthAcq0(roi).copy(
      exposure = 20.secTimeSpan, // Same as Acq0, not multiplied
      readout  = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi      = roi.slitRoi,
      fpu      = gmosNorthScience(0).fpu
    )

  override def gmosNorthAcq2(roi: GmosLongSlitAcquisitionRoi): GmosNorth =
    gmosNorthAcq1(roi).copy(
      exposure = 60.secTimeSpan // Match what runtime produces (20s * 3)
    )

  // Override ITC mock to detect blind offset targets by RV=999 km/s marker
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    // Check for the radial velocity marker (999 km/s)
    val isBlindOffset = input.asterism.exists: target =>
      RadialVelocity.kilometerspersecond.reverseGet(target.radialVelocity) == 999.0

    if (isBlindOffset) {
      fakeItcImagingResult.some
    } else {
      IntegrationTime(10.secTimeSpan, PosInt.unsafeFrom(6)).some
    }

  // Expected acquisition using blind offset timing (20s base → 20s, 20s, 60s)
  val BlindOffsetAcquisition: Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(0,  0)},
                  ${gmosNorthExpectedAcq(1, 10)},
                  ${gmosNorthExpectedAcq(2,  0, breakpoint = Breakpoint.Enabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${gmosNorthExpectedAcq(2, 0)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  // For comparison: expected acquisition using regular timing (10s base → 10s, 20s, 30s)
  test("acquisition with blind offset target uses blind offset for acquisition"):
    val setup =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createObservationWithBlindOffset(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = BlindOffsetAcquisition.asRight
      )

class executionGmosNorthWithoutBlindOffset extends ExecutionTestSupportForGmos:

  // Expected acquisition with regular timing (10s, 20s, 30s)
  val RegularAcquisition: Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(0, 0)},
                  ${gmosNorthExpectedAcq(1, 10)},
                  ${gmosNorthExpectedAcq(2, 0, breakpoint = Breakpoint.Enabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${gmosNorthExpectedAcq(2, 0)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  test("acquisition without blind offset target uses regular targets"):
    val setup =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = RegularAcquisition.asRight
      )
