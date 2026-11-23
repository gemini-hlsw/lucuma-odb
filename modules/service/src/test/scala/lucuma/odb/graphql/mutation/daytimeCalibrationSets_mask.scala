// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.GmosCustomMaskInput
import lucuma.odb.service.AttachmentMetadataService
import lucuma.odb.service.workflow.validator.DaytimeCalibrationValidator
import lucuma.odb.util.Codecs.*
import skunk.codec.text.text
import skunk.syntax.all.*

class daytimeCalibrationSets_mask extends OdbSuite
  with DatabaseOperations
  with DaytimeCalibrationOps
  with MosMaskSupport:

  val pi          = TestUsers.Standard.pi(1, 101)
  val staff       = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers = List(pi, staff, serviceUser)

  private def maskFpu(aid: Option[Attachment.Id], slitWidth: String): String =
    s"fpu: { customMask: { slitWidth: $slitWidth${aid.fold("")(a => s", attachmentId: ${a.asJson}")} } }"

  private def widthlessMaskFpu(aid: Attachment.Id): String =
    s"fpu: { customMask: { attachmentId: ${aid.asJson} } }"

  private def gmosSetup(site: String, fpu: String): String =
    s"""
      $site: {
        exposure: { seconds: 10 }
        readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: FAST }
        dtax: ZERO
        roi: FULL_FRAME
        $fpu
      }
    """

  private def flamingos2Setup(fpu: String): String =
    s"""
      flamingos2: {
        exposure: { seconds: 20 }
        filter: H
        readMode: BRIGHT
        lyotWheel: F16
        decker: MOS
        readoutMode: SCIENCE
        reads: READS_1
        $fpu
      }
    """

  private def setupFor(instrument: Instrument, aid: Option[Attachment.Id]): String =
    instrument match
      case Instrument.GmosNorth  => gmosSetup("gmosNorth", maskFpu(aid, "CUSTOM_WIDTH_0_50"))
      case Instrument.GmosSouth  => gmosSetup("gmosSouth", maskFpu(aid, "CUSTOM_WIDTH_0_50"))
      case Instrument.Flamingos2 => flamingos2Setup(maskFpu(aid, "CUSTOM_WIDTH_2_PIX"))
      case other                 => sys.error(s"No mask setup for $other")

  private def dark(setup: String): String =
    s"{ kind: DARK, explicitCount: 1, $setup }"

  private def selection(instrument: Instrument): String =
    val field = instrument match
      case Instrument.GmosNorth => "gmosNorth"
      case Instrument.GmosSouth => "gmosSouth"
      case _                    => "flamingos2"
    s"calibrationSets { $field { fpu { customMask { attachmentId slitWidth } builtin } } }"

  private def createQuery(pid: Program.Id, instrument: Instrument, sets: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            observingMode: {
              daytimeCalibration: { instrument: ${instrument.tag.toScreamingSnakeCase}, calibrationSets: [$sets] }
            }
          }
        }) {
          observation { observingMode { daytimeCalibration { ${selection(instrument)} } } }
        }
      }
    """

  private def updateQuery(oid: Observation.Id, sets: String): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { observingMode: { daytimeCalibration: { calibrationSets: [$sets] } } }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations { id }
        }
      }
    """

  private def expectRejected(query: String, fragment: String): IO[Unit] =
    expectOdbError(
      user     = staff,
      query    = query,
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains(fragment) => () }
    )

  private def storedMask(instrument: Instrument, json: Json): Json =
    val field = instrument match
      case Instrument.GmosNorth => "gmosNorth"
      case Instrument.GmosSouth => "gmosSouth"
      case _                    => "flamingos2"
    json.hcursor
      .downFields("createObservation", "observation", "observingMode", "daytimeCalibration", "calibrationSets")
      .downArray
      .downFields(field, "fpu", "customMask", "attachmentId")
      .require[Json]

  private def acceptsOwnMask(instrument: Instrument): IO[Unit] =
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, s"${instrument.tag}_ODF.fits", instrument)
      res <- query(staff, createQuery(cal, instrument, dark(setupFor(instrument, Some(aid)))))
    yield assertEquals(storedMask(instrument, res), aid.asJson)

  test("a GMOS North set takes a mask from its own program"):
    acceptsOwnMask(Instrument.GmosNorth)

  test("a GMOS South set takes a mask from its own program"):
    acceptsOwnMask(Instrument.GmosSouth)

  test("a Flamingos 2 set takes a mask from its own program"):
    acceptsOwnMask(Instrument.Flamingos2)

  test("a custom mask set without an attachment is rejected"):
    for
      cal <- createProgramAs(staff)
      _   <- expectRejected(
               createQuery(cal, Instrument.GmosNorth, dark(setupFor(Instrument.GmosNorth, None))),
               AttachmentMetadataService.DaytimeMaskRequiredMessage
             )
    yield ()

  test("a mask cut for another instrument is rejected"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "south_ODF.fits", Instrument.GmosSouth)
      _   <- expectRejected(
               createQuery(cal, Instrument.GmosNorth, dark(setupFor(Instrument.GmosNorth, Some(aid)))),
               "is designed for GMOS South"
             )
    yield ()

  test("an attachment that is not a MOS mask is rejected"):
    for
      cal <- createProgramAs(staff)
      aid <- insertObsAttachment(cal, "finder", "finder.png")
      _   <- expectRejected(
               createQuery(cal, Instrument.GmosNorth, dark(setupFor(Instrument.GmosNorth, Some(aid)))),
               AttachmentMetadataService.daytimeMaskNotFoundMessage(aid)
             )
    yield ()

  test("replacing the sets checks the mask too"):
    for
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      bad <- insertMosMaskAttachment(cal, "f2_ODF.fits", Instrument.Flamingos2)
      ok  <- insertMosMaskAttachment(cal, "north_ODF.fits", Instrument.GmosNorth)
      _   <- expectRejected(
               updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(bad)))),
               "is designed for Flamingos"
             )
      _   <- expectRejected(
               updateQuery(oid, dark(setupFor(Instrument.GmosNorth, None))),
               AttachmentMetadataService.DaytimeMaskRequiredMessage
             )
      _   <- expect(
               user     = staff,
               query    = updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(ok)))),
               expected = json"""{ "updateObservations": { "observations": [ { "id": $oid } ] } }""".asRight
             )
    yield ()

  test("a set through the pinhole builtin needs no attachment"):
    for
      cal <- createProgramAs(staff)
      res <- query(staff, createQuery(cal, Instrument.GmosNorth, dark(gmosSetup("gmosNorth", "fpu: { builtin: PINHOLE }"))))
    yield assertEquals(
      res.hcursor
        .downFields("createObservation", "observation", "observingMode", "daytimeCalibration", "calibrationSets")
        .downArray
        .downFields("gmosNorth", "fpu", "builtin")
        .require[Json],
      "PINHOLE".asJson
    )

  private def storedWidth(instrument: Instrument, json: Json): Json =
    val field = instrument match
      case Instrument.GmosNorth => "gmosNorth"
      case Instrument.GmosSouth => "gmosSouth"
      case _                    => "flamingos2"
    json.hcursor
      .downFields("createObservation", "observation", "observingMode", "daytimeCalibration", "calibrationSets")
      .downArray
      .downFields(field, "fpu", "customMask", "slitWidth")
      .require[Json]

  private def arcsec(d: Double): Angle =
    Angle.fromDoubleArcseconds(d)

  test("a GMOS set without a slit width takes the nearest width to the mask's"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "width_n_ODF.fits", Instrument.GmosNorth, averageSlitWidth = Some(arcsec(0.8)))
      res <- query(staff, createQuery(cal, Instrument.GmosNorth, dark(gmosSetup("gmosNorth", widthlessMaskFpu(aid)))))
    yield assertEquals(storedWidth(Instrument.GmosNorth, res), "CUSTOM_WIDTH_0_75".asJson)

  test("a Flamingos 2 set without a slit width takes the mask's width in pixels"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "width_f_ODF.fits", Instrument.Flamingos2, averageSlitWidth = Some(arcsec(0.72)))
      res <- query(staff, createQuery(cal, Instrument.Flamingos2, dark(flamingos2Setup(widthlessMaskFpu(aid)))))
    yield assertEquals(storedWidth(Instrument.Flamingos2, res), "CUSTOM_WIDTH_4_PIX".asJson)

  test("a slit width that is given wins over the mask's"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "width_s_ODF.fits", Instrument.GmosSouth, averageSlitWidth = Some(arcsec(0.8)))
      res <- query(staff, createQuery(cal, Instrument.GmosSouth, dark(gmosSetup("gmosSouth", maskFpu(Some(aid), "CUSTOM_WIDTH_2_00")))))
    yield assertEquals(storedWidth(Instrument.GmosSouth, res), "CUSTOM_WIDTH_2_00".asJson)

  test("a mask that gives no slit width needs one in the set"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "nowidth_ODF.fits", Instrument.GmosNorth)
      _   <- expectRejected(
               createQuery(cal, Instrument.GmosNorth, dark(gmosSetup("gmosNorth", widthlessMaskFpu(aid)))),
               AttachmentMetadataService.daytimeMaskSlitWidthMessage(aid)
             )
    yield ()

  test("replacing the sets takes the mask's slit width too"):
    for
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      aid <- insertMosMaskAttachment(cal, "width_u_ODF.fits", Instrument.GmosNorth, averageSlitWidth = Some(arcsec(1.4)))
      _   <- query(staff, updateQuery(oid, dark(gmosSetup("gmosNorth", widthlessMaskFpu(aid)))))
      res <- query(staff, s"""
               query {
                 observation(observationId: ${oid.asJson}) {
                   observingMode { daytimeCalibration { calibrationSets { gmosNorth { fpu { customMask { slitWidth } } } } } }
                 }
               }
             """)
    yield assertEquals(
      res.hcursor
        .downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets")
        .downArray
        .downFields("gmosNorth", "fpu", "customMask", "slitWidth")
        .require[Json],
      "CUSTOM_WIDTH_1_50".asJson
    )

  private def maskStep(aid: Option[Attachment.Id]): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 10 }
          readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: FAST }
          dtax: ZERO
          roi: FULL_FRAME
          ${maskFpu(aid, "CUSTOM_WIDTH_0_50")}
        }
        stepConfig: { dark: true }
        observeClass: DAY_CAL
      }
    """

  private def widthlessMaskStep(aid: Attachment.Id): String =
    maskStep(Some(aid)).replace("slitWidth: CUSTOM_WIDTH_0_50, ", "")

  private def replaceQuery(oid: Observation.Id, step: String): String =
    s"""
      mutation {
        replaceGmosNorthSequence(input: ${input(oid, SequenceType.Science, atomInput("Mask", step))}) {
          sequence { steps { instrumentConfig { fpu { customMask { attachmentId } } } } }
        }
      }
    """

  test("a hand-entered step takes a mask from its own program"):
    for
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      aid <- insertMosMaskAttachment(cal, "step_ODF.fits", Instrument.GmosNorth)
      _   <- expect(
               user     = staff,
               query    = replaceQuery(oid, maskStep(Some(aid))),
               expected = json"""
                 {
                   "replaceGmosNorthSequence": {
                     "sequence": [
                       { "steps": [ { "instrumentConfig": { "fpu": { "customMask": { "attachmentId": $aid } } } } ] }
                     ]
                   }
                 }
               """.asRight
             )
    yield ()

  test("a hand-entered step follows the same mask rules as a set"):
    for
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      bad <- insertMosMaskAttachment(cal, "wrong_ODF.fits", Instrument.GmosSouth)
      _   <- expectRejected(replaceQuery(oid, maskStep(None)), AttachmentMetadataService.DaytimeMaskRequiredMessage)
      _   <- expectRejected(replaceQuery(oid, maskStep(Some(bad))), "is designed for GMOS South")
    yield ()

  test("a hand-entered step still needs a slit width"):
    for
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      aid <- insertMosMaskAttachment(cal, "stepwidth_ODF.fits", Instrument.GmosNorth, averageSlitWidth = Some(arcsec(0.8)))
      _   <- expectRejected(replaceQuery(oid, widthlessMaskStep(aid)), GmosCustomMaskInput.SlitWidthRequiredMessage)
    yield ()

  private def deleteMask(aid: Attachment.Id): IO[Unit] =
    withSession(_.execute(sql"DELETE FROM t_attachment WHERE c_attachment_id = $attachment_id".command)(aid)).void

  private def workflow(pid: Program.Id, oid: Observation.Id): IO[(String, List[String])] =
    runObscalcUpdateAs(serviceUser, pid, oid) *>
      query(staff, workflowQuery(oid)).map: json =>
        val w = json.hcursor.downFields("observation", "workflow", "value")
        (
          w.downField("state").require[String],
          w.downField("validationErrors").require[List[Json]].flatMap(_.hcursor.downField("messages").require[List[String]])
        )

  test("a set whose mask was deleted is reported until another mask is chosen"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "gone_ODF.fits", Instrument.GmosNorth)
      oid <- createDaytimeCalibrationAs(staff, cal)
      _   <- query(staff, updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(aid)))))
      w0  <- workflow(cal, oid)
      _   <- deleteMask(aid)
      w1  <- workflow(cal, oid)
      aid2 <- insertMosMaskAttachment(cal, "new_ODF.fits", Instrument.GmosNorth)
      _   <- query(staff, updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(aid2)))))
      w2  <- workflow(cal, oid)
    yield
      assertEquals(w0, ("DEFINED", Nil))
      assertEquals(w1, ("UNDEFINED", List(DaytimeCalibrationValidator.maskMissing)))
      assertEquals(w2, ("DEFINED", Nil))

  private def recutMask(aid: Attachment.Id, instrument: Instrument): IO[Unit] =
    withSession(_.execute(sql"""
      UPDATE t_attachment
         SET c_mask_definition = jsonb_set(c_mask_definition, '{instrument}', to_jsonb($text::text))
       WHERE c_attachment_id = $attachment_id
    """.command)(instrument.tag.toScreamingSnakeCase, aid)).void

  test("a set whose mask was cut again for another instrument is reported"):
    for
      cal <- createProgramAs(staff)
      aid <- insertMosMaskAttachment(cal, "recut_ODF.fits", Instrument.GmosNorth)
      oid <- createDaytimeCalibrationAs(staff, cal)
      _   <- query(staff, updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(aid)))))
      w0  <- workflow(cal, oid)
      _   <- recutMask(aid, Instrument.GmosSouth)
      w1  <- workflow(cal, oid)
      _   <- recutMask(aid, Instrument.GmosNorth)
      w2  <- workflow(cal, oid)
    yield
      assertEquals(w0, ("DEFINED", Nil))
      assertEquals(w1, ("UNDEFINED", List(DaytimeCalibrationValidator.maskMismatch)))
      assertEquals(w2, ("DEFINED", Nil))

  test("a mask from another program is rejected, for a set and for a step"):
    for
      sci <- createProgramAs(pi)
      cal <- createProgramAs(staff)
      oid <- createDaytimeCalibrationAs(staff, cal)
      aid <- insertMosMaskAttachment(sci, "other_ODF.fits", Instrument.GmosNorth)
      _   <- expectRejected(updateQuery(oid, dark(setupFor(Instrument.GmosNorth, Some(aid)))), "belongs to another program")
      _   <- expectRejected(replaceQuery(oid, maskStep(Some(aid))), "belongs to another program")
    yield ()
