// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2MosOffsetPreset
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.flamingos2.defaultMosTelescopeConfigs
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.slit_offset_mode
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

class createObservation_Flamingos2Mos extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

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
    withSession(_.unique(q)(pid, fileName, fileName.stripSuffix("_ODF.fits").toUpperCase))

  private def scienceRequirements: String =
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

  private val simpleMode: String =
    """
      flamingos2Mos: {
        disperser: R1200_HK
        filter: H
        customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
      }
    """

  private val telescopeConfigsSelection: String =
    """
      offsetMode
      alongSlit { q { arcseconds } guiding }
      toSky { offset { p { arcseconds } q { arcseconds } } guiding }
    """

  private def modeQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
            mode
            flamingos2Mos {
              disperser
              filter
              customMask {
                slitWidth
                attachmentId
              }
              exposureTimeMode {
                signalToNoise { value at { nanometers } }
              }
              decker
              defaultDecker
              explicitDecker
              readoutMode
              defaultReadoutMode
              telluricType { tag }
              acquisition {
                filter
                defaultFilter
                explicitFilter
                exposureTimeMode {
                  signalToNoise { value at { nanometers } }
                }
              }
              initialDisperser
              initialFilter
              initialSlitWidth
            }
          }
        }
      }
    """

  test("create Flamingos 2 MOS, no mask attachment"):
    setup(simpleMode).flatMap: (_, oid) =>
      expect(pi, modeQuery(oid), json"""
        {
          "observation": {
            "observingMode": {
              "mode": "FLAMINGOS_2_MOS",
              "flamingos2Mos": {
                "disperser": "R1200_HK",
                "filter": "H",
                "customMask": {
                  "slitWidth": "CUSTOM_WIDTH_2_PIX",
                  "attachmentId": null
                },
                "exposureTimeMode": {
                  "signalToNoise": {
                    "value": 100.000,
                    "at": { "nanometers": 2100.000 }
                  }
                },
                "decker": "MOS",
                "defaultDecker": "MOS",
                "explicitDecker": null,
                "readoutMode": "SCIENCE",
                "defaultReadoutMode": "SCIENCE",
                "telluricType": { "tag": "HOT" },
                "acquisition": {
                  "filter": "H",
                  "defaultFilter": "H",
                  "explicitFilter": null,
                  "exposureTimeMode": {
                    "signalToNoise": {
                      "value": 10.000,
                      "at": { "nanometers": 2100.000 }
                    }
                  }
                },
                "initialDisperser": "R1200_HK",
                "initialFilter": "H",
                "initialSlitWidth": "CUSTOM_WIDTH_2_PIX"
              }
            }
          }
        }
      """.asRight)

  test("create Flamingos 2 MOS with a mask attachment"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits")
      oid <- create(pid, tid, s"""
               flamingos2Mos: {
                 disperser: R1200_HK
                 filter: H
                 customMask: {
                   slitWidth: CUSTOM_WIDTH_4_PIX
                   attachmentId: "$aid"
                 }
               }
             """)
      _   <- expect(pi, s"""
               query {
                 observation(observationId: "$oid") {
                   observingMode {
                     flamingos2Mos {
                       customMask { slitWidth attachmentId }
                     }
                   }
                 }
               }
             """, json"""
               {
                 "observation": {
                   "observingMode": {
                     "flamingos2Mos": {
                       "customMask": {
                         "slitWidth": "CUSTOM_WIDTH_4_PIX",
                         "attachmentId": ${aid.asJson}
                       }
                     }
                   }
                 }
               }
             """.asRight)
    yield ()

  private def acquisitionQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
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
          }
        }
      }
    """

  test("the acquisition filter and exposure time mode may be specified"):
    setup("""
      flamingos2Mos: {
        disperser: R1200_HK
        filter: H
        customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
        acquisition: {
          explicitFilter: K_SHORT
          exposureTimeMode: {
            signalToNoise: { value: 25.0, at: { nanometers: 2200 } }
          }
        }
      }
    """).flatMap: (_, oid) =>
      expect(pi, acquisitionQuery(oid), json"""
        {
          "observation": {
            "observingMode": {
              "flamingos2Mos": {
                "acquisition": {
                  "filter": "K_SHORT",
                  "defaultFilter": "H",
                  "explicitFilter": "K_SHORT",
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
        }
      """.asRight)

  test("only J, H and K_SHORT are accepted as acquisition filters"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user     = pi,
               query    = s"""
                 mutation {
                   createObservation(input: {
                     programId: ${pid.asJson}
                     SET: {
                       targetEnvironment: { asterism: ${List(tid).asJson} }
                       scienceRequirements: { $scienceRequirements }
                       observingMode: {
                         flamingos2Mos: {
                           disperser: R1200_HK
                           filter: H
                           customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
                           acquisition: { explicitFilter: JH }
                         }
                       }
                     }
                   }) {
                     observation { id }
                   }
                 }
               """,
               expected = List("Argument 'input.SET.observingMode.flamingos2Mos.acquisition' is invalid: 'explicitFilter' must contain one of: J, H, K_SHORT").asLeft
             )
    yield ()

  test("the default nods along the slit at +/- 1.2 arcsec"):
    setup(simpleMode).flatMap: (_, oid) =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              flamingos2Mos {
                telescopeConfigs { $telescopeConfigsSelection }
                defaultTelescopeConfigs { $telescopeConfigsSelection }
                explicitTelescopeConfigs { $telescopeConfigsSelection }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "flamingos2Mos": {
                "telescopeConfigs": {
                  "offsetMode": "NOD_ALONG_SLIT",
                  "alongSlit": [
                    { "q": { "arcseconds":  1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds": -1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds": -1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds":  1.200000 }, "guiding": "ENABLED" }
                  ],
                  "toSky": null
                },
                "defaultTelescopeConfigs": {
                  "offsetMode": "NOD_ALONG_SLIT",
                  "alongSlit": [
                    { "q": { "arcseconds":  1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds": -1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds": -1.200000 }, "guiding": "ENABLED" },
                    { "q": { "arcseconds":  1.200000 }, "guiding": "ENABLED" }
                  ],
                  "toSky": null
                },
                "explicitTelescopeConfigs": null
              }
            }
          }
        }
      """.asRight)

  // A crowded field nods to sky, which the client asks for by writing the configs
  // explicitly from lucuma-core's defaultMosTelescopeConfigs, as for IGRINS 2.
  test("explicit nod-to-sky configs keep guiding off on the sky offsets"):
    setup("""
      flamingos2Mos: {
        disperser: R1200_HK
        filter: H
        customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
        explicitTelescopeConfigs: {
          toSky: [
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:   0.0 } }, guiding: ENABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 300.0 } }, guiding: DISABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 320.0 } }, guiding: DISABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:   0.0 } }, guiding: ENABLED }
          ]
        }
      }
    """).flatMap: (_, oid) =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              flamingos2Mos {
                telescopeConfigs { $telescopeConfigsSelection }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "flamingos2Mos": {
                "telescopeConfigs": {
                  "offsetMode": "NOD_TO_SKY",
                  "alongSlit": null,
                  "toSky": [
                    { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":   0.000000 } }, "guiding": "ENABLED" },
                    { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 300.000000 } }, "guiding": "DISABLED" },
                    { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 320.000000 } }, "guiding": "DISABLED" },
                    { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds":   0.000000 } }, "guiding": "ENABLED" }
                  ]
                }
              }
            }
          }
        }
      """.asRight)

  test("explicit overrides round-trip"):
    setup("""
      flamingos2Mos: {
        disperser: R1200_HK
        filter: H
        customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
        explicitReadMode: BRIGHT
        explicitReads: READS_4
        explicitDecker: LONG_SLIT
        explicitReadoutMode: ENGINEERING
        telluricType: { tag: A0V }
      }
    """).flatMap: (_, oid) =>
      expect(pi, s"""
        query {
          observation(observationId: "$oid") {
            observingMode {
              flamingos2Mos {
                explicitReadMode
                explicitReads
                decker
                defaultDecker
                explicitDecker
                readoutMode
                explicitReadoutMode
                telluricType { tag }
              }
            }
          }
        }
      """, json"""
        {
          "observation": {
            "observingMode": {
              "flamingos2Mos": {
                "explicitReadMode": "BRIGHT",
                "explicitReads": "READS_4",
                "decker": "LONG_SLIT",
                "defaultDecker": "MOS",
                "explicitDecker": "LONG_SLIT",
                "readoutMode": "ENGINEERING",
                "explicitReadoutMode": "ENGINEERING",
                "telluricType": { "tag": "A0V" }
              }
            }
          }
        }
      """.asRight)

  // The view's defaults are a literal copy of lucuma-core's, so a core change
  // would otherwise drift from the database silently.
  private def readDefaultTelescopeConfigs(oid: Observation.Id): IO[SlitTelescopeConfigs] =
    val q: Query[Observation.Id, (SlitOffsetMode, String)] =
      sql"""
        SELECT c_slit_offset_mode_default, c_telescope_configs_default
        FROM v_flamingos_2_mos
        WHERE c_observation_id = $observation_id
      """.query(slit_offset_mode *: text)
    withSession(_.unique(q)(oid)).map: stored =>
      SlitTelescopeConfigsFormat.getOption(stored).getOrElse(sys.error(s"Could not parse '$stored'."))

  test("the stored default matches lucuma-core's sparse field"):
    for
      (_, oid) <- setup(simpleMode)
      stored   <- readDefaultTelescopeConfigs(oid)
      _        <- IO(assertEquals(stored, defaultMosTelescopeConfigs(Flamingos2MosOffsetPreset.SparseField)))
    yield ()

  test("OTHER is rejected as a custom slit width"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      _   <- expect(
               user  = pi,
               query = s"""
                 mutation {
                   createObservation(input: {
                     programId: ${pid.asJson}
                     SET: {
                       targetEnvironment: { asterism: ${List(tid).asJson} }
                       scienceRequirements: { $scienceRequirements }
                       observingMode: {
                         flamingos2Mos: {
                           disperser: R1200_HK
                           filter: H
                           customMask: { slitWidth: OTHER }
                         }
                       }
                     }
                   }) {
                     observation { id }
                   }
                 }
               """,
               expected = List("Argument 'input.SET.observingMode.flamingos2Mos' is invalid: Flamingos 2 MOS does not support the 'OTHER' custom slit width.").asLeft
             )
    yield ()
