// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.program_id
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

// Shared by the suites below, which cannot be one suite because the delete test
// additionally needs S3 for the attachment route.
trait CustomMaskOps extends ReplaceSequenceOps:
  self: query.ExecutionTestSupport =>

  protected val SlitWidth = "CUSTOM_WIDTH_0_50"

  // Insert a mos_mask attachment directly rather than going through the file
  // service and S3, so the setup depends only on the database.
  protected def insertMosMaskAttachment(pid: Program.Id, fileName: String): IO[Attachment.Id] =
    val q: Query[(Program.Id, String), Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_file_size,
          c_remote_path
        )
        VALUES ($program_id, 'mos_mask', $text, 42, 'unused')
        RETURNING c_attachment_id
      """.query(attachment_id)
    withSession(_.unique(q)(pid, fileName))

  protected def gmosStep(customMask: String): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 20 }
          readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
          dtax: ZERO
          roi: FULL_FRAME
          gratingConfig: { grating: R831_G5302, order: ZERO, wavelength: { nanometers: 500.0 } }
          filter: R_PRIME
          fpu: { customMask: $customMask }
        }
        stepConfig: { science: true }
        observeClass: SCIENCE
      }
    """

class customMask extends query.ExecutionTestSupportForGmos with CustomMaskOps:

  private def stepRecordQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            atomRecords {
              matches {
                steps {
                  matches {
                    gmosNorth {
                      fpu {
                        customMask {
                          attachmentId
                          slitWidth
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

  private def expected(attachmentId: Option[Attachment.Id]) =
    json"""
      {
        "observation": {
          "execution": {
            "atomRecords": {
              "matches": [
                {
                  "steps": {
                    "matches": [
                      {
                        "gmosNorth": {
                          "fpu": {
                            "customMask": {
                              "attachmentId": ${attachmentId.map(_.toString)},
                              "slitWidth": $SlitWidth
                            }
                          }
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        }
      }
    """

  // Every stage needed for a recorded step to show up in atomRecords.
  private val StepStages: List[StepStage] =
    List(
      StepStage.StartStep,
      StepStage.StartConfigure,
      StepStage.EndConfigure,
      StepStage.StartObserve,
      StepStage.EndObserve,
      StepStage.EndStep
    )

  private def stepRecordTest(state: String, defined: Boolean): Unit =
    test(s"$state custom mask survives the step record read path"):
      for
        p   <- createProgram
        t   <- createTargetWithProfileAs(pi, p)
        o   <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        a   <- Option.when(defined)(insertMosMaskAttachment(p, "mask.fits")).sequence
        // Pending simply omits the attachment id; the slit width alone marks
        // the custom mask as present.
        cm   = a.fold(s"""{ slitWidth: $SlitWidth }""")(id => s"""{ attachmentId: "$id", slitWidth: $SlitWidth }""")
        in   = input(o, SequenceType.Science, atomInput("Recorded", gmosStep(cm)))
        res <- query(pi, mutation(Instrument.GmosNorth, in))
        sid  = mutationOutput(Instrument.GmosNorth, res).head._2.head
        vid <- recordVisitAs(serviceUser, o)
        _   <- StepStages.traverse(addStepEventAs(serviceUser, sid, vid, _))
        _   <- expect(pi, stepRecordQuery(o), expected(a).asRight)
      yield ()

  stepRecordTest("defined", true)
  stepRecordTest("pending", false)
