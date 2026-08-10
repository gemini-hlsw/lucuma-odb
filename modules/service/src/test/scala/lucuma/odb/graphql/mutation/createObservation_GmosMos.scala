// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.program_id
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

class createObservation_GmosMos extends OdbSuite:

  val pi: StandardUser    = TestUsers.Standard.pi(nextId, nextId)
  val staff: StandardUser = TestUsers.Standard.staff(nextId, nextId)

  lazy val validUsers: List[User] = List(pi, staff)

  // Insert the attachment directly rather than through the file service and S3,
  // so the setup depends only on the database.
  protected def insertMosMaskAttachment(pid: Program.Id, fileName: String): IO[Attachment.Id] =
    val q: Query[(Program.Id, String, String), Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path,
          c_mask_name
        )
        VALUES ($program_id, 'mos_mask', $text, 42, 'unused', $text)
        RETURNING c_attachment_id
      """.query(attachment_id)
    withSession(_.unique(q)(pid, fileName, fileName.stripSuffix(".fits")))

  private def scienceRequirements: String =
    """
      exposureTimeMode: {
        signalToNoise: {
          value: 100.0
          at: { nanometers: 510 }
        }
      }
      spectroscopy: {
        wavelength: { nanometers: 500 }
        resolution: 100
        wavelengthCoverage: { nanometers: 20 }
        focalPlane: MULTIPLE_SLIT
        focalPlaneAngle: { microarcseconds: 0 }
      }
    """

  private def create(
    pid:  Program.Id,
    tid:  Target.Id,
    mode: String
  ): IO[Observation.Id] =
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

  private def setup(mode: String): IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, mode)
    yield (pid, oid)

  private val northMode: String =
    """
      gmosNorthMos: {
        grating: R831_G5302
        filter: R_PRIME
        customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
        centralWavelength: { nanometers: 500 }
      }
    """

  private val southMode: String =
    """
      gmosSouthMos: {
        grating: B1200_G5321
        filter: R_PRIME
        customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
        centralWavelength: { nanometers: 500 }
      }
    """

  private def modeQuery(oid: Observation.Id, field: String): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
            mode
            $field {
              grating
              filter
              customMask {
                slitWidth
                attachmentId
              }
              centralWavelength { nanometers }
              acquisitionType
              initialGrating
              initialFilter
              initialSlitWidth
              initialCentralWavelength { nanometers }
              roi
              ampGain
              ampReadMode
            }
          }
        }
      }
    """

  test("create GMOS North MOS, no mask attachment"):
    setup(northMode).flatMap: (_, oid) =>
      expect(pi, modeQuery(oid, "gmosNorthMos"), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GMOS_NORTH_MOS",
              "gmosNorthMos": {
                "grating": "R831_G5302",
                "filter": "R_PRIME",
                "customMask": {
                  "slitWidth": "CUSTOM_WIDTH_1_00",
                  "attachmentId": null
                },
                "centralWavelength": { "nanometers": 500.000 },
                "acquisitionType": "MASK_IN",
                "initialGrating": "R831_G5302",
                "initialFilter": "R_PRIME",
                "initialSlitWidth": "CUSTOM_WIDTH_1_00",
                "initialCentralWavelength": { "nanometers": 500.000 },
                "roi": "FULL_FRAME",
                "ampGain": "LOW",
                "ampReadMode": "SLOW"
              }
            }
          }
        }
      """.asRight)

  test("create GMOS South MOS, no mask attachment"):
    setup(southMode).flatMap: (_, oid) =>
      expect(pi, modeQuery(oid, "gmosSouthMos"), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "GMOS_SOUTH_MOS",
              "gmosSouthMos": {
                "grating": "B1200_G5321",
                "filter": "R_PRIME",
                "customMask": {
                  "slitWidth": "CUSTOM_WIDTH_1_00",
                  "attachmentId": null
                },
                "centralWavelength": { "nanometers": 500.000 },
                "acquisitionType": "MASK_IN",
                "initialGrating": "B1200_G5321",
                "initialFilter": "R_PRIME",
                "initialSlitWidth": "CUSTOM_WIDTH_1_00",
                "initialCentralWavelength": { "nanometers": 500.000 },
                "roi": "FULL_FRAME",
                "ampGain": "LOW",
                "ampReadMode": "SLOW"
              }
            }
          }
        }
      """.asRight)

  test("create GMOS North MOS with a mask attachment"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "mask.fits")
      oid <- create(pid, tid, s"""
               gmosNorthMos: {
                 grating: R831_G5302
                 customMask: {
                   slitWidth: CUSTOM_WIDTH_0_50
                   attachmentId: "$aid"
                 }
                 centralWavelength: { nanometers: 500 }
               }
             """)
      _   <- expect(pi, s"""
               query {
                 observation(observationId: "$oid") {
                   observingMode {
                     gmosNorthMos {
                       customMask { slitWidth attachmentId }
                     }
                   }
                 }
               }
             """, json"""
               {
                 "observation": {
                   "observingMode": {
                     "gmosNorthMos": {
                       "customMask": {
                         "slitWidth": "CUSTOM_WIDTH_0_50",
                         "attachmentId": ${aid.asJson}
                       }
                     }
                   }
                 }
               }
             """.asRight)
    yield ()

  test("explicit overrides round-trip"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, """
               gmosNorthMos: {
                 grating: R831_G5302
                 customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
                 centralWavelength: { nanometers: 500 }
                 explicitXBin: FOUR
                 explicitYBin: FOUR
                 explicitAmpReadMode: FAST
                 explicitAmpGain: HIGH
                 explicitRoi: CENTRAL_SPECTRUM
                 explicitWavelengthDithers: [
                   { nanometers: 0 },
                   { nanometers: 5 }
                 ]
                 explicitOffsets: [
                   { arcseconds: 0 },
                   { arcseconds: 10 }
                 ]
               }
             """)
      _   <- expect(pi, s"""
               query {
                 observation(observationId: "$oid") {
                   observingMode {
                     gmosNorthMos {
                       xBin
                       explicitXBin
                       yBin
                       explicitYBin
                       ampReadMode
                       explicitAmpReadMode
                       ampGain
                       explicitAmpGain
                       roi
                       explicitRoi
                       wavelengthDithers { nanometers }
                       explicitWavelengthDithers { nanometers }
                       offsets { arcseconds }
                       explicitOffsets { arcseconds }
                     }
                   }
                 }
               }
             """, json"""
               {
                 "observation": {
                   "observingMode": {
                     "gmosNorthMos": {
                       "xBin": "FOUR",
                       "explicitXBin": "FOUR",
                       "yBin": "FOUR",
                       "explicitYBin": "FOUR",
                       "ampReadMode": "FAST",
                       "explicitAmpReadMode": "FAST",
                       "ampGain": "HIGH",
                       "explicitAmpGain": "HIGH",
                       "roi": "CENTRAL_SPECTRUM",
                       "explicitRoi": "CENTRAL_SPECTRUM",
                       "wavelengthDithers": [
                         { "nanometers": 0.000 },
                         { "nanometers": 5.000 }
                       ],
                       "explicitWavelengthDithers": [
                         { "nanometers": 0.000 },
                         { "nanometers": 5.000 }
                       ],
                       "offsets": [
                         { "arcseconds": 0.000000 },
                         { "arcseconds": 10.000000 }
                       ],
                       "explicitOffsets": [
                         { "arcseconds": 0.000000 },
                         { "arcseconds": 10.000000 }
                       ]
                     }
                   }
                 }
               }
             """.asRight)
    yield ()

  test("acquisitionType is explicitly settable on create"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, """
               gmosNorthMos: {
                 grating: R831_G5302
                 customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
                 centralWavelength: { nanometers: 500 }
                 acquisitionType: MASK_OUT
               }
             """)
      _   <- expect(pi, s"""
               query {
                 observation(observationId: "$oid") {
                   observingMode {
                     gmosNorthMos { acquisitionType }
                   }
                 }
               }
             """, json"""
               {
                 "observation": {
                   "observingMode": {
                     "gmosNorthMos": { "acquisitionType": "MASK_OUT" }
                   }
                 }
               }
             """.asRight)
    yield ()

  test("an explicit acquisition filter round-trips"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, """
               gmosNorthMos: {
                 grating: R831_G5302
                 customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
                 centralWavelength: { nanometers: 500 }
                 acquisition: { explicitFilter: I_PRIME }
               }
             """)
      _   <- expect(pi, s"""
               query {
                 observation(observationId: "$oid") {
                   observingMode {
                     gmosNorthMos {
                       acquisition { filter explicitFilter }
                     }
                   }
                 }
               }
             """, json"""
               {
                 "observation": {
                   "observingMode": {
                     "gmosNorthMos": {
                       "acquisition": {
                         "filter": "I_PRIME",
                         "explicitFilter": "I_PRIME"
                       }
                     }
                   }
                 }
               }
             """.asRight)
    yield ()

  private def expectRejectedAcquisitionFilter(
    mode:    String,
    grating: String,
    filter:  String,
    allowed: String
  ): IO[Unit] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user = pi,
               query = s"""
                 mutation {
                   createObservation(input: {
                     programId: ${pid.asJson}
                     SET: {
                       targetEnvironment: { asterism: ${List(tid).asJson} }
                       scienceRequirements: { $scienceRequirements }
                       observingMode: {
                         $mode: {
                           grating: $grating
                           customMask: { slitWidth: CUSTOM_WIDTH_1_00 }
                           centralWavelength: { nanometers: 500 }
                           acquisition: { explicitFilter: $filter }
                         }
                       }
                     }
                   }) {
                     observation { id }
                   }
                 }
               """,
               expected = List(s"Argument 'input.SET.observingMode.$mode.acquisition' is invalid: 'explicitFilter' must contain one of: $allowed").asLeft
             )
    yield ()

  test("a non-acquisition explicit filter is rejected (North)"):
    expectRejectedAcquisitionFilter("gmosNorthMos", "R831_G5302", "GG455", "G_PRIME, R_PRIME, I_PRIME")

  test("a non-acquisition explicit filter is rejected (South)"):
    expectRejectedAcquisitionFilter("gmosSouthMos", "B1200_G5321", "GG455", "U_PRIME, G_PRIME, R_PRIME, I_PRIME")

  test("defaults come from the grating and the custom mask"):
    setup(northMode).flatMap: (_, oid) =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              gmosNorthMos {
                defaultWavelengthDithers { nanometers }
                defaultOffsets { arcseconds }
                wavelengthDithers { nanometers }
                offsets { arcseconds }
                explicitWavelengthDithers { nanometers }
                explicitOffsets { arcseconds }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "gmosNorthMos": {
                "defaultWavelengthDithers": [
                  { "nanometers": 0.000 },
                  { "nanometers": 5.000 },
                  { "nanometers": -5.000 }
                ],
                "defaultOffsets": [],
                "wavelengthDithers": [
                  { "nanometers": 0.000 },
                  { "nanometers": 5.000 },
                  { "nanometers": -5.000 }
                ],
                "offsets": [],
                "explicitWavelengthDithers": null,
                "explicitOffsets": null
              }
            }
          }
        }
      """.asRight)

  test("customMask is required"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user = pi,
               query = s"""
                 mutation {
                   createObservation(input: {
                     programId: ${pid.asJson}
                     SET: {
                       targetEnvironment: { asterism: ${List(tid).asJson} }
                       scienceRequirements: { $scienceRequirements }
                       observingMode: {
                         gmosNorthMos: {
                           grating: R831_G5302
                           centralWavelength: { nanometers: 500 }
                         }
                       }
                     }
                   }) {
                     observation { id }
                   }
                 }
               """,
               expected = List("Argument 'input.SET.observingMode.gmosNorthMos' is invalid: A customMask is required in order to create a GMOS North MOS observing mode.").asLeft
             )
    yield ()

  // A MOS and a long slit observation with the same grating must not share a request.
  test("a MOS request is distinct from a long slit request with the same grating"):
    for
      cfp  <- createGeminiCallForProposalsAs(staff)
      pid  <- createProgramAs(pi)
      _    <- addProposal(pi, pid, cfp.some, None)
      tid  <- createTargetAs(pi, pid)
      mos  <- create(pid, tid, northMode)
      ls   <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
      rMos <- createConfigurationRequestAs(pi, mos)
      rLs  <- createConfigurationRequestAs(pi, ls)
      _    <- assertIO(IO(rMos =!= rLs), true, "MOS and long slit shared a configuration request")
    yield ()
