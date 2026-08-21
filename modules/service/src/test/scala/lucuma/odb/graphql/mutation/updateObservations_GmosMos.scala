// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Instrument
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.service.AttachmentMetadataService
import lucuma.odb.service.GmosMosService
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.attachment_type
import lucuma.odb.util.Codecs.observation_id
import skunk.Query
import skunk.syntax.all.*

class updateObservations_GmosMos extends OdbSuite with MosMaskSupport:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  // The mask attachment is stored as two columns (id + type) pinned together
  // by a composite foreign key, and the type column is not exposed via GraphQL,
  // so clearing the mask can only be fully verified against the row itself.
  private def readNorthMaskColumns(oid: Observation.Id): IO[(Option[Attachment.Id], Option[AttachmentType])] =
    val q: Query[Observation.Id, (Option[Attachment.Id], Option[AttachmentType])] =
      sql"""
        SELECT c_mask_attachment_id, c_mask_attachment_type
        FROM t_gmos_north_mos
        WHERE c_observation_id = $observation_id
      """.query((attachment_id.opt *: attachment_type.opt).map((aid, tpe) => (aid, tpe)))
    withSession(_.unique(q)(oid))

  private val scienceRequirements: String =
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

  private def northMode(mask: String): String =
    s"""
      gmosNorthMos: {
        grating: R831_G5302
        filter: R_PRIME
        customMask: { $mask }
        centralWavelength: { nanometers: 500 }
      }
    """

  private def southMode(mask: String): String =
    s"""
      gmosSouthMos: {
        grating: B1200_G5321
        filter: R_PRIME
        customMask: { $mask }
        centralWavelength: { nanometers: 500 }
      }
    """

  private def updateMutation(oid: Observation.Id, mode: String, selection: String): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { $mode } }
        }) {
          observations {
            observingMode { $selection }
          }
        }
      }
    """

  private def setupNorth(mask: String): IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, northMode(mask))
    yield (pid, oid)

  test("attach a mask to a maskless observation"):
    for
      (pid, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      _          <- expect(pi, updateMutation(
                      oid,
                      s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid" } }""",
                      "gmosNorthMos { customMask { slitWidth attachmentId } }"
                    ), json"""
                      {
                        "updateObservations": {
                          "observations": [
                            {
                              "observingMode": {
                                "gmosNorthMos": {
                                  "customMask": {
                                    "slitWidth": "CUSTOM_WIDTH_1_00",
                                    "attachmentId": ${aid.asJson}
                                  }
                                }
                              }
                            }
                          ]
                        }
                      }
                    """.asRight)
    yield ()

  test("clear the mask attachment"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      oid <- create(pid, tid, northMode(s"""slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid""""))
      _   <- expect(pi, updateMutation(
               oid,
               "gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: null } }",
               "gmosNorthMos { customMask { slitWidth attachmentId } }"
             ), json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosNorthMos": {
                           "customMask": {
                             "slitWidth": "CUSTOM_WIDTH_1_00",
                             "attachmentId": null
                           }
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
      _    <- readNorthMaskColumns(oid).assertEquals((Option.empty[Attachment.Id], Option.empty[AttachmentType]))
    yield ()

  // The mask is a whole value, so supplying it replaces it outright.
  test("editing the slit width without resending the attachment clears it"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      oid <- create(pid, tid, northMode(s"""slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid""""))
      _   <- expect(pi, updateMutation(
               oid,
               "gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_0_25 } }",
               "gmosNorthMos { customMask { slitWidth attachmentId } }"
             ), json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosNorthMos": {
                           "customMask": {
                             "slitWidth": "CUSTOM_WIDTH_0_25",
                             "attachmentId": null
                           }
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
      _    <- readNorthMaskColumns(oid).assertEquals((Option.empty[Attachment.Id], Option.empty[AttachmentType]))
    yield ()

  test("resending the attachment alongside a new slit width keeps it"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      oid <- create(pid, tid, northMode(s"""slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid""""))
      _   <- expect(pi, updateMutation(
               oid,
               s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_0_25, attachmentId: "$aid" } }""",
               "gmosNorthMos { customMask { slitWidth attachmentId } }"
             ), json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosNorthMos": {
                           "customMask": {
                             "slitWidth": "CUSTOM_WIDTH_0_25",
                             "attachmentId": ${aid.asJson}
                           }
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
    yield ()

  test("edit the grating, leaving the mask alone"):
    for
      (_, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      _        <- expect(pi, updateMutation(
                    oid,
                    "gmosNorthMos: { grating: B1200_G5301 }",
                    "gmosNorthMos { grating initialGrating customMask { slitWidth } }"
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "gmosNorthMos": {
                                "grating": "B1200_G5301",
                                "initialGrating": "R831_G5302",
                                "customMask": { "slitWidth": "CUSTOM_WIDTH_1_00" }
                              }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
    yield ()

  test("edit the acquisition type"):
    for
      (_, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      _        <- expect(pi, updateMutation(
                    oid,
                    "gmosNorthMos: { acquisitionType: MASK_OUT }",
                    "gmosNorthMos { acquisitionType }"
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "gmosNorthMos": { "acquisitionType": "MASK_OUT" }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
    yield ()

  test("GMOS South round-trips too"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosSouth)
      oid <- create(pid, tid, southMode("slitWidth: CUSTOM_WIDTH_1_00"))
      _   <- expect(pi, updateMutation(
               oid,
               s"""gmosSouthMos: { customMask: { slitWidth: CUSTOM_WIDTH_2_00, attachmentId: "$aid" } }""",
               "gmosSouthMos { customMask { slitWidth attachmentId } }"
             ), json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosSouthMos": {
                           "customMask": {
                             "slitWidth": "CUSTOM_WIDTH_2_00",
                             "attachmentId": ${aid.asJson}
                           }
                         }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
    yield ()

  test("an attachment of the wrong type is rejected"):
    for
      (pid, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      aid        <- insertObsAttachment(pid, "finder", "finder.fits")
      _          <- expect(
                      user     = pi,
                      query    = updateMutation(
                        oid,
                        s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid" } }""",
                        "gmosNorthMos { customMask { attachmentId } }"
                      ),
                      expected = List(GmosMosService.MaskAttachmentViolationMessage).asLeft
                    )
    yield ()

  test("a rejected attachment leaves the observation unchanged"):
    for
      (pid, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      aid        <- insertObsAttachment(pid, "finder", "finder.fits")
      _          <- expect(
                      user     = pi,
                      query    = updateMutation(
                        oid,
                        s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_0_25, attachmentId: "$aid" } }""",
                        "gmosNorthMos { customMask { attachmentId } }"
                      ),
                      expected = List(GmosMosService.MaskAttachmentViolationMessage).asLeft
                    )
      _          <- expect(pi, s"""
                      query {
                        observation(observationId: "$oid") {
                          observingMode {
                            gmosNorthMos { customMask { slitWidth attachmentId } }
                          }
                        }
                      }
                    """, json"""
                      {
                        "observation": {
                          "observingMode": {
                            "gmosNorthMos": {
                              "customMask": {
                                "slitWidth": "CUSTOM_WIDTH_1_00",
                                "attachmentId": null
                              }
                            }
                          }
                        }
                      }
                    """.asRight)
    yield ()

  test("an attachment from another program is rejected"):
    for
      (_, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      other    <- createProgramAs(pi)
      aid      <- insertMosMaskAttachment(other, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      _        <- expect(
                    user     = pi,
                    query    = updateMutation(
                      oid,
                      s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid" } }""",
                      "gmosNorthMos { customMask { attachmentId } }"
                    ),
                    expected = List(GmosMosService.MaskAttachmentViolationMessage).asLeft
                  )
    yield ()

  test("creating with an attachment from another program is rejected"):
    for
      pid   <- createProgramAs(pi)
      tid   <- createTargetAs(pi, pid)
      other <- createProgramAs(pi)
      aid   <- insertMosMaskAttachment(other, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      _     <- expect(
                 user     = pi,
                 query    = s"""
                   mutation {
                     createObservation(input: {
                       programId: ${pid.asJson}
                       SET: {
                         targetEnvironment: { asterism: ${List(tid).asJson} }
                         scienceRequirements: { $scienceRequirements }
                         observingMode: { ${northMode(s"slitWidth: CUSTOM_WIDTH_1_00, attachmentId: \"$aid\"")} }
                       }
                     }) {
                       observation { id }
                     }
                   }
                 """,
                 expected = List(GmosMosService.MaskAttachmentViolationMessage).asLeft
               )
    yield ()

  private def mismatch(maskName: String, mask: Instrument, obs: Instrument): List[String] =
    List(AttachmentMetadataService.maskInstrumentMismatchMessage(
      NonEmptyString.unsafeFrom(maskName),
      mask,
      obs
    ))

  test("a GMOS South mask is rejected on a GMOS North observation"):
    for
      (pid, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      aid        <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.GmosSouth)
      _          <- expect(
                      user     = pi,
                      query    = updateMutation(
                        oid,
                        s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid" } }""",
                        "gmosNorthMos { customMask { attachmentId } }"
                      ),
                      expected = mismatch("GS2025AQ001-01", Instrument.GmosSouth, Instrument.GmosNorth).asLeft
                    )
      // The refused update leaves the observation without a mask.
      _          <- readNorthMaskColumns(oid).assertEquals((Option.empty[Attachment.Id], Option.empty[AttachmentType]))
    yield ()

  test("creating with a GMOS South mask on a GMOS North observation is rejected"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.GmosSouth)
      _   <- expect(
               user     = pi,
               query    = s"""
                 mutation {
                   createObservation(input: {
                     programId: ${pid.asJson}
                     SET: {
                       targetEnvironment: { asterism: ${List(tid).asJson} }
                       scienceRequirements: { $scienceRequirements }
                       observingMode: { ${northMode(s"slitWidth: CUSTOM_WIDTH_1_00, attachmentId: \"$aid\"")} }
                     }
                   }) {
                     observation { id }
                   }
                 }
               """,
               expected = mismatch("GS2025AQ001-01", Instrument.GmosSouth, Instrument.GmosNorth).asLeft
             )
    yield ()

  // The only mask mutation test with more than one observation, so the only one
  // that exercises the pre-check's multi-id fragment.
  test("a mask is validated against every observation in a batch update"):
    for
      (pid, oid1) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      tid         <- createTargetAs(pi, pid)
      oid2        <- create(pid, tid, northMode("slitWidth: CUSTOM_WIDTH_1_00"))
      aid         <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.GmosSouth)
      _           <- expect(
                       user     = pi,
                       query    = s"""
                         mutation {
                           updateObservations(input: {
                             WHERE: { id: { IN: ["$oid1", "$oid2"] } }
                             SET: { observingMode: { gmosNorthMos: { customMask: {
                               slitWidth: CUSTOM_WIDTH_1_00
                               attachmentId: "$aid"
                             } } } }
                           }) {
                             observations { id }
                           }
                         }
                       """,
                       expected = mismatch("GS2025AQ001-01", Instrument.GmosSouth, Instrument.GmosNorth).asLeft
                     )
      _           <- readNorthMaskColumns(oid1).assertEquals((Option.empty[Attachment.Id], Option.empty[AttachmentType]))
      _           <- readNorthMaskColumns(oid2).assertEquals((Option.empty[Attachment.Id], Option.empty[AttachmentType]))
    yield ()

  test("switching an observation to the other GMOS instrument with its mask is rejected"):
    for
      (pid, oid) <- setupNorth("slitWidth: CUSTOM_WIDTH_1_00")
      aid        <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      _          <- query(pi, updateMutation(
                      oid,
                      s"""gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid" } }""",
                      "gmosNorthMos { customMask { attachmentId } }"
                    ))
      _          <- expect(
                      user     = pi,
                      query    = updateMutation(
                        oid,
                        southMode(s"""slitWidth: CUSTOM_WIDTH_1_00, attachmentId: "$aid""""),
                        "gmosSouthMos { customMask { attachmentId } }"
                      ),
                      expected = mismatch("GN2025AQ001-01", Instrument.GmosNorth, Instrument.GmosSouth).asLeft
                    )
    yield ()
