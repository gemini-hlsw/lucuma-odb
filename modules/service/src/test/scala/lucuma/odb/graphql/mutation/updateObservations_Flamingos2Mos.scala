// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.service.Flamingos2MosService
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.attachment_type
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

class updateObservations_Flamingos2Mos extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private def insertAttachment(pid: Program.Id, tpe: String, fileName: String): IO[Attachment.Id] =
    val q: Query[(Program.Id, String, String, Option[String]), Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path,
          c_mask_name
        )
        VALUES ($program_id, $text::e_attachment_type, $text, 42, 'unused', ${text.opt})
        RETURNING c_attachment_id
      """.query(attachment_id)
    val maskName = Option.when(tpe === "mos_mask")(fileName.stripSuffix("_ODF.fits").toUpperCase)
    withSession(_.unique(q)(pid, tpe, fileName, maskName))

  // The mask attachment is stored as two columns (id + type) pinned together
  // by a composite foreign key, and the type column is not exposed via GraphQL,
  // so clearing the mask can only be fully verified against the row itself.
  private def readMaskColumns(oid: Observation.Id): IO[(Option[Attachment.Id], Option[AttachmentType])] =
    val q: Query[Observation.Id, (Option[Attachment.Id], Option[AttachmentType])] =
      sql"""
        SELECT c_mask_attachment_id, c_mask_attachment_type
        FROM t_flamingos_2_mos
        WHERE c_observation_id = $observation_id
      """.query((attachment_id.opt *: attachment_type.opt).map((aid, tpe) => (aid, tpe)))
    withSession(_.unique(q)(oid))

  private val scienceRequirements: String =
    """
      exposureTimeMode: {
        signalToNoise: {
          value: 100.0
          at: { nanometers: 2100 }
        }
      }
      spectroscopy: {
        wavelength: { nanometers: 2100 }
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

  private def mode(mask: String): String =
    s"""
      flamingos2Mos: {
        disperser: R1200_HK
        filter: H
        customMask: { $mask }
      }
    """

  private def updateMutation(oid: Observation.Id, set: String, selection: String): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { $set } }
        }) {
          observations {
            observingMode { $selection }
          }
        }
      }
    """

  private def setup(mask: String): IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- create(pid, tid, mode(mask))
    yield (pid, oid)

  test("attach a mask to a maskless observation"):
    for
      (pid, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      aid        <- insertAttachment(pid, "mos_mask", "GS2025AQ001-01_ODF.fits")
      _          <- expect(pi, updateMutation(
                      oid,
                      s"""flamingos2Mos: { customMask: { slitWidth: CUSTOM_WIDTH_2_PIX, attachmentId: "$aid" } }""",
                      "flamingos2Mos { customMask { slitWidth attachmentId } }"
                    ), json"""
                      {
                        "updateObservations": {
                          "observations": [
                            {
                              "observingMode": {
                                "flamingos2Mos": {
                                  "customMask": {
                                    "slitWidth": "CUSTOM_WIDTH_2_PIX",
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
      pid  <- createProgramAs(pi)
      tid  <- createTargetAs(pi, pid)
      aid  <- insertAttachment(pid, "mos_mask", "GS2025AQ001-01_ODF.fits")
      oid  <- create(pid, tid, mode(s"""slitWidth: CUSTOM_WIDTH_2_PIX, attachmentId: "$aid""""))
      _    <- expect(pi, updateMutation(
                oid,
                "flamingos2Mos: { customMask: { slitWidth: CUSTOM_WIDTH_2_PIX, attachmentId: null } }",
                "flamingos2Mos { customMask { slitWidth attachmentId } }"
              ), json"""
                {
                  "updateObservations": {
                    "observations": [
                      {
                        "observingMode": {
                          "flamingos2Mos": {
                            "customMask": {
                              "slitWidth": "CUSTOM_WIDTH_2_PIX",
                              "attachmentId": null
                            }
                          }
                        }
                      }
                    ]
                  }
                }
              """.asRight)
      cols <- readMaskColumns(oid)
      _    <- IO(assertEquals(cols, (Option.empty[Attachment.Id], Option.empty[AttachmentType])))
    yield ()

  private val AcquisitionSelection: String =
    """
      flamingos2Mos {
        acquisition {
          filter
          defaultFilter
          explicitFilter
          exposureTimeMode {
            signalToNoise { value at { nanometers } }
          }
        }
      }
    """

  test("override the acquisition filter and exposure time mode, then unset the filter"):
    for
      (_, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      _        <- expect(pi, updateMutation(
                    oid,
                    """
                      flamingos2Mos: {
                        acquisition: {
                          explicitFilter: J
                          exposureTimeMode: {
                            signalToNoise: { value: 25.0, at: { nanometers: 2200 } }
                          }
                        }
                      }
                    """,
                    AcquisitionSelection
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "flamingos2Mos": {
                                "acquisition": {
                                  "filter": "J",
                                  "defaultFilter": "H",
                                  "explicitFilter": "J",
                                  "exposureTimeMode": {
                                    "signalToNoise": {
                                      "value": 25.000,
                                      "at": { "nanometers": 2200.000 }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
      _        <- expect(pi, updateMutation(
                    oid,
                    "flamingos2Mos: { acquisition: { explicitFilter: null } }",
                    AcquisitionSelection
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "flamingos2Mos": {
                                "acquisition": {
                                  "filter": "H",
                                  "defaultFilter": "H",
                                  "explicitFilter": null,
                                  "exposureTimeMode": {
                                    "signalToNoise": {
                                      "value": 25.000,
                                      "at": { "nanometers": 2200.000 }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
    yield ()

  test("an explicit override wins over the default, and null restores it"):
    for
      (_, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      _        <- expect(pi, updateMutation(
                    oid,
                    """
                      flamingos2Mos: {
                        explicitTelescopeConfigs: {
                          toSky: [
                            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:  0.0 } }, guiding: ENABLED },
                            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 60.0 } }, guiding: DISABLED },
                            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 70.0 } }, guiding: DISABLED },
                            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:  0.0 } }, guiding: ENABLED }
                          ]
                        }
                      }
                    """,
                    """
                      flamingos2Mos {
                        telescopeConfigs {
                          offsetMode
                          toSky { offset { q { arcseconds } } guiding }
                        }
                      }
                    """
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "flamingos2Mos": {
                                "telescopeConfigs": {
                                  "offsetMode": "NOD_TO_SKY",
                                  "toSky": [
                                    { "offset": { "q": { "arcseconds":  0.000000 } }, "guiding": "ENABLED" },
                                    { "offset": { "q": { "arcseconds": 60.000000 } }, "guiding": "DISABLED" },
                                    { "offset": { "q": { "arcseconds": 70.000000 } }, "guiding": "DISABLED" },
                                    { "offset": { "q": { "arcseconds":  0.000000 } }, "guiding": "ENABLED" }
                                  ]
                                }
                              }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
      _        <- expect(pi, updateMutation(
                    oid,
                    "flamingos2Mos: { explicitTelescopeConfigs: null }",
                    """
                      flamingos2Mos {
                        explicitTelescopeConfigs { offsetMode }
                        telescopeConfigs { offsetMode }
                      }
                    """
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "flamingos2Mos": {
                                "explicitTelescopeConfigs": null,
                                "telescopeConfigs": { "offsetMode": "NOD_ALONG_SLIT" }
                              }
                            }
                          }
                        ]
                      }
                    }
                  """.asRight)
    yield ()

  test("edit the disperser, leaving the mask alone"):
    for
      (_, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      _        <- expect(pi, updateMutation(
                    oid,
                    "flamingos2Mos: { disperser: R1200_JH }",
                    "flamingos2Mos { disperser initialDisperser customMask { slitWidth } }"
                  ), json"""
                    {
                      "updateObservations": {
                        "observations": [
                          {
                            "observingMode": {
                              "flamingos2Mos": {
                                "disperser": "R1200_JH",
                                "initialDisperser": "R1200_HK",
                                "customMask": { "slitWidth": "CUSTOM_WIDTH_2_PIX" }
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
      (pid, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      aid        <- insertAttachment(pid, "finder", "finder.fits")
      _          <- expect(
                      user     = pi,
                      query    = updateMutation(
                        oid,
                        s"""flamingos2Mos: { customMask: { slitWidth: CUSTOM_WIDTH_2_PIX, attachmentId: "$aid" } }""",
                        "flamingos2Mos { customMask { attachmentId } }"
                      ),
                      expected = List(Flamingos2MosService.MaskAttachmentViolationMessage).asLeft
                    )
      cols       <- readMaskColumns(oid)
      _          <- IO(assertEquals(cols, (Option.empty[Attachment.Id], Option.empty[AttachmentType])))
    yield ()

  test("an attachment from another program is rejected"):
    for
      (_, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      other    <- createProgramAs(pi)
      aid      <- insertAttachment(other, "mos_mask", "GS2025AQ001-01_ODF.fits")
      _        <- expect(
                    user     = pi,
                    query    = updateMutation(
                      oid,
                      s"""flamingos2Mos: { customMask: { slitWidth: CUSTOM_WIDTH_2_PIX, attachmentId: "$aid" } }""",
                      "flamingos2Mos { customMask { attachmentId } }"
                    ),
                    expected = List(Flamingos2MosService.MaskAttachmentViolationMessage).asLeft
                  )
    yield ()

  test("OTHER is rejected as a custom slit width"):
    for
      (_, oid) <- setup("slitWidth: CUSTOM_WIDTH_2_PIX")
      _        <- expect(
                    user     = pi,
                    query    = updateMutation(
                      oid,
                      "flamingos2Mos: { customMask: { slitWidth: OTHER } }",
                      "flamingos2Mos { customMask { slitWidth } }"
                    ),
                    expected = List("Argument 'input.SET.observingMode.flamingos2Mos' is invalid: Flamingos 2 MOS does not support the 'OTHER' custom slit width.").asLeft
                  )
    yield ()
