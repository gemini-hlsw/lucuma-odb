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
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The per-mode `gnirsLongSlit` / `gnirsIfu` inputs and output fields, which
 * replace the combined `gnirsSpectroscopy` with its `slit` / `ifu` members. Both
 * spellings write the same table, so the two must be interchangeable.
 */
class createObservation_GnirsSplitSpectroscopy extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi)

  private def scienceRequirements(focalPlane: String): String =
    s"""
      spectroscopy: {
        wavelength: { nanometers: 2200 }
        resolution: 1000
        wavelengthCoverage: { nanometers: 200 }
        focalPlane: $focalPlane
        focalPlaneAngle: { microarcseconds: 0 }
      }
    """

  private val centralWavelengths: String =
    """
      centralWavelengths: [
        {
          centralWavelength: { nanometers: 2200 }
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 30.0 }
              count: 3
              at: { nanometers: 2200 }
            }
          }
        }
      ]
    """

  private def createMutation(
    pid:        Program.Id,
    tid:        Target.Id,
    mode:       String,
    focalPlane: String
  ): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            targetEnvironment: { asterism: ${List(tid).asJson} }
            scienceRequirements: { ${scienceRequirements(focalPlane)} }
            observingMode: { $mode }
          }
        }) {
          observation { id }
        }
      }
    """

  private def create(mode: String, focalPlane: String = "SINGLE_SLIT"): IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- query(pi, createMutation(pid, tid, mode, focalPlane))
               .map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])
    yield oid

  private def expectCreateFailure(mode: String, message: String, focalPlane: String = "SINGLE_SLIT"): IO[Unit] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(pi, createMutation(pid, tid, mode, focalPlane), List(message).asLeft)
    yield ()

  private val longSlitMode: String =
    s"""
      gnirsLongSlit: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        fpu: LONG_SLIT_0_30
        filter: ORDER3
        $centralWavelengths
      }
    """

  private val ifuMode: String =
    s"""
      gnirsIfu: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        fpu: LOW_RESOLUTION
        filter: ORDER3
        $centralWavelengths
      }
    """

  private def modeQuery(oid: Observation.Id, selection: String): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode { $selection }
        }
      }
    """

  test("create GNIRS long slit via gnirsLongSlit"):
    create(longSlitMode).flatMap: oid =>
      expect(pi, modeQuery(oid, """
        mode
        gnirsLongSlit { grating prism camera fpu initialFpu filter decker }
      """), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GNIRS_LONG_SLIT",
              "gnirsLongSlit": {
                "grating": "D111",
                "prism": "MIRROR",
                "camera": "SHORT_BLUE",
                "fpu": "LONG_SLIT_0_30",
                "initialFpu": "LONG_SLIT_0_30",
                "filter": "ORDER3",
                "decker": "SHORT_CAM_LONG_SLIT"
              }
            }
          }
        }
      """.asRight)

  test("create GNIRS IFU via gnirsIfu"):
    create(ifuMode, focalPlane = "IFU").flatMap: oid =>
      expect(pi, modeQuery(oid, """
        mode
        gnirsIfu { grating camera fpu initialFpu decker defaultDecker }
      """), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GNIRS_IFU",
              "gnirsIfu": {
                "grating": "D111",
                "camera": "SHORT_BLUE",
                "fpu": "LOW_RESOLUTION",
                "initialFpu": "LOW_RESOLUTION",
                "decker": "LOW_RESOLUTION_IFU",
                "defaultDecker": "LOW_RESOLUTION_IFU"
              }
            }
          }
        }
      """.asRight)

  test("the mode that does not apply resolves to null"):
    for
      ls  <- create(longSlitMode)
      ifu <- create(ifuMode, focalPlane = "IFU")
      _   <- expect(pi, modeQuery(ls, "gnirsLongSlit { fpu } gnirsIfu { fpu }"), json"""
               {
                 "observation": {
                   "observingMode": {
                     "gnirsLongSlit": { "fpu": "LONG_SLIT_0_30" },
                     "gnirsIfu": null
                   }
                 }
               }
             """.asRight)
      _   <- expect(pi, modeQuery(ifu, "gnirsLongSlit { fpu } gnirsIfu { fpu }"), json"""
               {
                 "observation": {
                   "observingMode": {
                     "gnirsLongSlit": null,
                     "gnirsIfu": { "fpu": "LOW_RESOLUTION" }
                   }
                 }
               }
             """.asRight)
    yield ()

  test("a mode created with the deprecated input reads back through the new fields"):
    create(s"""
      gnirsSpectroscopy: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        slit: { fpu: LONG_SLIT_0_45 }
        filter: ORDER3
        $centralWavelengths
      }
    """).flatMap: oid =>
      expect(pi, modeQuery(oid, "gnirsLongSlit { fpu } gnirsSpectroscopy { slit { fpu } }"), json"""
        {
          "observation": {
            "observingMode": {
              "gnirsLongSlit": { "fpu": "LONG_SLIT_0_45" },
              "gnirsSpectroscopy": { "slit": { "fpu": "LONG_SLIT_0_45" } }
            }
          }
        }
      """.asRight)

  test("a mode created with the new input reads back through the deprecated field"):
    create(ifuMode, focalPlane = "IFU").flatMap: oid =>
      expect(pi, modeQuery(oid, "gnirsSpectroscopy { slit { fpu } ifu { fpu } }"), json"""
        {
          "observation": {
            "observingMode": {
              "gnirsSpectroscopy": {
                "slit": null,
                "ifu": { "fpu": "LOW_RESOLUTION" }
              }
            }
          }
        }
      """.asRight)

  test("edit through gnirsLongSlit"):
    for
      oid <- create(longSlitMode)
      _   <- expect(pi, s"""
               mutation {
                 updateObservations(input: {
                   SET: {
                     observingMode: {
                       gnirsLongSlit: {
                         fpu: LONG_SLIT_0_45
                         explicitDecker: SHORT_CAM_LONG_SLIT
                       }
                     }
                   }
                   WHERE: { id: { EQ: ${oid.asJson} } }
                 }) {
                   observations {
                     observingMode {
                       gnirsLongSlit { fpu initialFpu explicitDecker camera }
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
                         "gnirsLongSlit": {
                           "fpu": "LONG_SLIT_0_45",
                           "initialFpu": "LONG_SLIT_0_30",
                           "explicitDecker": "SHORT_CAM_LONG_SLIT",
                           "camera": "SHORT_BLUE"
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
    yield ()

  test("fpu is required on create"):
    expectCreateFailure(s"""
      gnirsLongSlit: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        filter: ORDER3
        $centralWavelengths
      }
    """, "Argument 'input.SET.observingMode.gnirsLongSlit' is invalid: A fpu is required to create a GNIRS spectroscopy observing mode.")

  test("gnirsLongSlit cannot be combined with the deprecated gnirsSpectroscopy"):
    expectCreateFailure(s"""
      $longSlitMode
      gnirsSpectroscopy: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        slit: { fpu: LONG_SLIT_0_30 }
        filter: ORDER3
        $centralWavelengths
      }
    """, "Exactly one key must be specified for oneOf input object ObservingModeInput in field 'createObservation' of type 'Mutation', but found 'gnirsLongSlit', 'gnirsSpectroscopy'")
