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
import lucuma.core.model.Target
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.program_id
import skunk.Query
import skunk.codec.text.text
import skunk.syntax.all.*

// These tests guard the V1230 migration rather than a user-facing feature.
trait CustomMaskOps extends ReplaceSequenceOps:
  self: query.ExecutionTestSupport =>

  // Insert a mos_mask attachment directly rather than going through the file
  // service and S3, so the test depends only on the database.
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

  protected val GmosSlitWidth       = "CUSTOM_WIDTH_0_50"
  protected val Flamingos2SlitWidth = "CUSTOM_WIDTH_4_PIX"

  protected def gmosStep(grating: String)(customMask: String): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 20 }
          readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
          dtax: ZERO
          roi: FULL_FRAME
          gratingConfig: { grating: $grating, order: ZERO, wavelength: { nanometers: 500.0 } }
          filter: R_PRIME
          fpu: { customMask: $customMask }
        }
        stepConfig: { science: true }
        observeClass: SCIENCE
      }
    """

  protected def flamingos2Step(customMask: String): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 20 }
          filter: J
          readMode: BRIGHT
          lyotWheel: F16
          decker: LONG_SLIT
          readoutMode: SCIENCE
          reads: READS_1
          fpu: { customMask: $customMask }
        }
        stepConfig: { science: true }
        observeClass: SCIENCE
      }
    """

  protected def stepRecordQuery(oid: Observation.Id, instrumentField: String): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            atomRecords {
              matches {
                steps {
                  matches {
                    $instrumentField {
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

  protected def expectedStepRecord(
    instrumentField: String,
    slitWidth:       String,
    attachmentId:    Option[Attachment.Id]
  ) =
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
                        $instrumentField: {
                          "fpu": {
                            "customMask": {
                              "attachmentId": ${attachmentId.map(_.toString)},
                              "slitWidth": $slitWidth
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

  protected val StepStages: List[StepStage] =
    List(
      StepStage.StartStep,
      StepStage.StartConfigure,
      StepStage.EndConfigure,
      StepStage.StartObserve,
      StepStage.EndObserve,
      StepStage.EndStep
    )

class customMask extends query.ExecutionTestSupportForGmos with CustomMaskOps:
  protected def customMaskTests(
    label:           String,
    instrument:      Instrument,
    instrumentField: String,
    slitWidth:       String,
    step:            String => String,
    createObs:       (Program.Id, List[Target.Id]) => IO[Observation.Id]
  ): Unit =

    def stepRecordTest(state: String, defined: Boolean): Unit =
      test(s"$label - $state custom mask survives the step record read path"):
        for
          p   <- createProgram
          t   <- createTargetWithProfileAs(pi, p)
          o   <- createObs(p, List(t))
          a   <- Option.when(defined)(insertMosMaskAttachment(p, s"$label-recorded.fits")).sequence
          // Pending simply omits the attachment id; the slit width alone marks
          // the custom mask as present.
          cm   = a.fold(s"""{ slitWidth: $slitWidth }""")(id => s"""{ attachmentId: "$id", slitWidth: $slitWidth }""")
          in   = input(o, SequenceType.Science, atomInput("Recorded", step(cm)))
          res <- query(pi, mutation(instrument, in))
          sid  = mutationOutput(instrument, res).head._2.head
          vid <- recordVisitAs(serviceUser, o)
          _   <- StepStages.traverse(addStepEventAs(serviceUser, sid, vid, _))
          _   <- expect(pi, stepRecordQuery(o, instrumentField), expectedStepRecord(instrumentField, slitWidth, a).asRight)
        yield ()

    stepRecordTest("defined", true)
    stepRecordTest("pending", false)

  customMaskTests(
    "gmos north",
    Instrument.GmosNorth,
    "gmosNorth",
    GmosSlitWidth,
    gmosStep("R831_G5302"),
    (p, ts) => createGmosNorthLongSlitObservationAs(pi, p, ts)
  )

  customMaskTests(
    "gmos south",
    Instrument.GmosSouth,
    "gmosSouth",
    GmosSlitWidth,
    gmosStep("R831_G5322"),
    (p, ts) => createGmosSouthLongSlitObservationAs(pi, p, ts)
  )

class customMaskFlamingos2 extends query.ExecutionTestSupportForFlamingos2 with CustomMaskOps:

  customMaskTests(
    "flamingos2",
    Instrument.Flamingos2,
    "flamingos2",
    Flamingos2SlitWidth,
    flamingos2Step,
    (p, ts) => createFlamingos2LongSlitObservationAs(pi, p, ts)
  )
