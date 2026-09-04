// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The GMOS IFU acquisition ROI (sc-10044).  It pairs the ROI for the field image with the ROI used
 * through the IFU, and its default depends on the observation's calibration role, so an explicit
 * choice has to survive an update and a clear has to fall back rather than stick.
 */
class updateObservations_GmosIfuAcquisition extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private val scienceRequirements: String =
    """
      exposureTimeMode: {
        signalToNoise: { value: 100.0, at: { nanometers: 510 } }
      }
      spectroscopy: {
        wavelength: { nanometers: 500 }
        resolution: 100
        wavelengthCoverage: { nanometers: 20 }
        focalPlane: SINGLE_SLIT
        focalPlaneAngle: { microarcseconds: 0 }
      }
    """

  private val northMode: String =
    """
      gmosNorthIfu: {
        grating: R831_G5302
        filter: R_PRIME
        fpu: TWO_SLITS
        centralWavelength: { nanometers: 500 }
      }
    """

  private def create(pid: Program.Id, tid: Target.Id, mode: String): IO[Observation.Id] =
    query(
      user  = pi,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              targetEnvironment: { asterism: ${List(tid).asJson} }
              scienceRequirements: { $scienceRequirements }
              observingMode: { $mode }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def setup: IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, northMode)
    yield oid

  private def updateMutation(oid: Observation.Id, mode: String): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { $mode } }
        }) {
          observations {
            observingMode {
              gmosNorthIfu { acquisition { roi defaultRoi explicitRoi } }
            }
          }
        }
      }
    """

  private def expected(roi: String, explicit: String) =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gmosNorthIfu": {
                  "acquisition": {
                    "roi": ${io.circe.Json.fromString(roi)},
                    "defaultRoi": "CCD2_FULL_FRAME",
                    "explicitRoi": ${io.circe.parser.parse(explicit).toOption.get}
                  }
                }
              }
            }
          ]
        }
      }
    """

  test("set an explicit acquisition ROI"):
    setup.flatMap: oid =>
      expect(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: FULL_FRAME } }"),
        expected("FULL_FRAME", "\"FULL_FRAME\"").asRight)

  test("switch between explicit acquisition ROIs"):
    for
      oid <- setup
      _   <- query(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: FULL_FRAME } }"))
      _   <- expect(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: STAMP_FULL_FRAME } }"),
               expected("STAMP_FULL_FRAME", "\"STAMP_FULL_FRAME\"").asRight)
    yield ()

  // Null clears the override, so the effective ROI falls back to the calibration-role default.
  test("clearing the acquisition ROI reverts to the default"):
    for
      oid <- setup
      _   <- query(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: FULL_FRAME } }"))
      _   <- expect(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: null } }"),
               expected("CCD2_FULL_FRAME", "null").asRight)
    yield ()

  // The acquisition filter and ROI live in separate columns; setting one must not disturb the other.
  test("the acquisition filter and ROI are independent"):
    setup.flatMap: oid =>
      for
        _ <- query(pi, updateMutation(oid, "gmosNorthIfu: { acquisition: { explicitRoi: FULL_FRAME } }"))
        _ <- expect(pi, s"""
               mutation {
                 updateObservations(input: {
                   WHERE: { id: { EQ: "$oid" } }
                   SET: { observingMode: { gmosNorthIfu: { acquisition: { explicitFilter: I_PRIME } } } }
                 }) {
                   observations {
                     observingMode {
                       gmosNorthIfu { acquisition { filter explicitFilter roi explicitRoi } }
                     }
                   }
                 }
               }
             """, json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosNorthIfu": {
                           "acquisition": {
                             "filter": "I_PRIME",
                             "explicitFilter": "I_PRIME",
                             "roi": "FULL_FRAME",
                             "explicitRoi": "FULL_FRAME"
                           }
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
      yield ()
