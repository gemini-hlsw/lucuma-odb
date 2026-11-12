// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.PerProgramPerConfigCalibrationsService
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.util.Codecs.attachment_id
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.Session
import skunk.codec.all.text
import skunk.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_mosMaskFlat
  extends ExecutionTestSupportForGmos
  with TelluricCalibrationsTestSupport
  with MosMaskSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  // A custom mask is looked up as its equivalent long slit, which the mirror and
  // r rows of the GMOS tables cover.  Flamingos-2 has no row without a disperser
  // for a long slit, so its flat is a Missing Smart GCAL Calibration.
  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some: s =>
    val gn: Gmos.TableKey.North = Gmos.TableKey(none, GmosNorthFilter.RPrime.some, GmosNorthFpu.LongSlit_0_50.some, GmosXBinning.One, GmosYBinning.One, GmosAmpGain.Low)
    val gn1: Gmos.TableKey.North = gn.copy(fpu = GmosNorthFpu.LongSlit_1_00.some)
    val gs: Gmos.TableKey.South = Gmos.TableKey(none, GmosSouthFilter.RPrime.some, GmosSouthFpu.LongSlit_0_50.some, GmosXBinning.One, GmosYBinning.One, GmosAmpGain.Low)
    super.dbInitialization.traverse_(_(s)) *>
      servicesFor(pi).map(_(s)).use: services =>
        services.transactionally:
          Services.asSuperUser:
            services.smartGcalService.insertGmosNorth(200, Gmos.TableRow(PosLong.unsafeFrom(1), gn, gmos_flat)) *>
              services.smartGcalService.insertGmosNorth(201, Gmos.TableRow(PosLong.unsafeFrom(1), gn1, gmos_flat)) *>
              services.smartGcalService.insertGmosSouth(200, Gmos.TableRow(PosLong.unsafeFrom(1), gs, gmos_flat))

  private def setMask(oid: Observation.Id, aid: Option[Attachment.Id], width: String = "CUSTOM_WIDTH_0_50"): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { observingMode: { gmosNorthMos: { customMask: { slitWidth: $width, attachmentId: ${aid.fold("null")(_.asJson.noSpaces)} } } } }
            }) {
              observations { id }
            }
          }"""
    ).void

  private def slitWidthOf(oid: Observation.Id): IO[String] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              observingMode { gmosNorthMos { customMask { slitWidth } } }
            }
          }"""
    ).flatMap(_.hcursor.downFields("observation", "observingMode", "gmosNorthMos", "customMask", "slitWidth").as[String].liftTo[IO])

  private def flatsOf(pid: Program.Id): IO[List[Observation.Id]] =
    queryCalibrationsWithRole(pid, CalibrationRole.MosMaskFlat)

  private def maskOfFlat(oid: Observation.Id, mode: String): IO[Option[Attachment.Id]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              observingMode { $mode { customMask { attachmentId } } }
            }
          }"""
    ).map(_.hcursor.downFields("observation", "observingMode", mode, "customMask", "attachmentId").as[Option[Attachment.Id]].toOption.flatten)

  private def replaceFile(aid: Attachment.Id, path: String): IO[Unit] =
    withSession(_.execute(sql"UPDATE t_attachment SET c_remote_path = $text WHERE c_attachment_id = $attachment_id".command)((path, aid)).void)

  private def deleteMask(aid: Attachment.Id): IO[Unit] =
    withSession(_.execute(sql"DELETE FROM t_attachment WHERE c_attachment_id = $attachment_id".command)(aid).void)

  private def queuedForCalibration(pid: Program.Id): IO[List[Observation.Id]] =
    withSession(_.execute(sql"SELECT c_observation_id FROM t_calibration_calc WHERE c_program_id = $program_id".query(observation_id))(pid))

  private def start(oid: Observation.Id): IO[Unit] =
    recordVisitAs(serviceUser, oid).flatMap(addSequenceEventAs(serviceUser, _, SequenceCommand.Start)).void

  private def recalc(pid: Program.Id, oid: Observation.Id): IO[(List[Observation.Id], List[Observation.Id])] =
    runObscalcUpdate(pid, oid) *> recalculateCalibrations(pid, when, oid)

  private def gnMosWithMask(name: String = "GN2025AQ001-01_ODF.fits"): IO[(Program.Id, Target.Id, Attachment.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, name, Instrument.GmosNorth)
      oid <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
    yield (pid, tid, aid, oid)

  test("a used GMOS North mask gets one flat in the Calibrations group"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      flats              <- flatsOf(pid)
      gid                <- queryObservation(flats.head).map(_.groupId)
      group              <- gid.traverse(queryGroupNameAndRoles)
      mask               <- maskOfFlat(flats.head, "gmosNorthMos")
    yield
      assertEquals(flats.size, 1)
      assertEquals(group.map(_.name), PerProgramPerConfigCalibrationsService.CalibrationsGroupName.value.some)
      assertEquals(mask, aid.some)

  test("observations sharing a mask share one flat; a second mask gets its own"):
    for
      (pid, tid, aid, oid) <- gnMosWithMask()
      _                    <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
      other                <- insertMosMaskAttachment(pid, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth)
      _                    <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(other.some))
      _                    <- recalc(pid, oid)
      flats                <- flatsOf(pid)
      masks                <- flats.traverse(maskOfFlat(_, "gmosNorthMos"))
    yield assertEquals(masks.flatten.toSet, Set(aid, other))

  test("a second calibration pass keeps the one flat"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalc(pid, oid)
      before           <- flatsOf(pid)
      (added, removed) <- recalculateCalibrations(pid, when, oid)
      after            <- flatsOf(pid)
    yield
      assertEquals(after, before)
      assert(!added.exists(before.contains) && !removed.exists(before.contains))

  test("no flat for a mask no observation uses"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      _   <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth)
      oid <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(none))
      _   <- recalc(pid, oid)
      fs  <- flatsOf(pid)
    yield assertEquals(fs, Nil)

  test("no flat in a calibration program, but one in an engineering program"):
    for
      (cal, _, _, calObs) <- gnMosWithMask()
      _                   <- setProgramReference(staff, cal, """calibration: { semester: "2025B", instrument: GMOS_NORTH }""")
      _                   <- recalc(cal, calObs)
      calFlats            <- flatsOf(cal)
      (eng, _, _, engObs) <- gnMosWithMask()
      _                   <- setProgramReference(staff, eng, """engineering: { semester: "2025B", instrument: GMOS_NORTH }""")
      _                   <- recalc(eng, engObs)
      engFlats            <- flatsOf(eng)
    yield
      assertEquals(calFlats, Nil)
      assertEquals(engFlats.size, 1)

  test("a mask used only by an inactive observation loses its flat"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalc(pid, oid)
      before           <- flatsOf(pid)
      _                <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Inactive)
      _                <- recalculateCalibrations(pid, when, oid)
      after            <- flatsOf(pid)
    yield
      assertEquals(before.size, 1)
      assertEquals(after, Nil)

  test("a mask used only by a completed observation loses its flat"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalc(pid, oid)
      before           <- flatsOf(pid)
      _                <- declareExecutionComplete(oid)
      _                <- recalc(pid, oid)
      after            <- flatsOf(pid)
    yield
      assertEquals(before.size, 1)
      assertEquals(after, Nil)

  test("unassigning the mask removes an untaken flat but keeps a started one"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      flat1              <- flatsOf(pid)
      _                  <- setMask(oid, none)
      _                  <- recalc(pid, oid)
      gone               <- flatsOf(pid)
      _                  <- setMask(oid, aid.some)
      _                  <- recalc(pid, oid)
      flat2              <- flatsOf(pid)
      _                  <- start(flat2.head)
      _                  <- setMask(oid, none)
      _                  <- recalc(pid, oid)
      kept               <- flatsOf(pid)
    yield
      assertEquals(flat1.size, 1)
      assertEquals(gone, Nil)
      assertEquals(kept, flat2)

  test("deleting the mask removes an untaken flat"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      before             <- flatsOf(pid)
      _                  <- deleteMask(aid)
      _                  <- recalc(pid, oid)
      after              <- flatsOf(pid)
    yield
      assertEquals(before.size, 1)
      assertEquals(after, Nil)

  test("a replaced mask file queues a recalculation for the observations using it"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      _                  <- withSession(_.execute(sql"DELETE FROM t_calibration_calc WHERE c_program_id = $program_id".command)(pid))
      _                  <- replaceFile(aid, "replaced")
      queued             <- queuedForCalibration(pid)
    yield assertEquals(queued, List(oid))

  test("a replaced file keeps an untaken flat and adds a new one beside a started flat"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      first              <- flatsOf(pid)
      _                  <- replaceFile(aid, "second")
      _                  <- recalculateCalibrations(pid, when, oid)
      same               <- flatsOf(pid)
      _                  <- start(first.head)
      _                  <- replaceFile(aid, "third")
      _                  <- recalculateCalibrations(pid, when, oid)
      both               <- flatsOf(pid)
    yield
      assertEquals(same, first)
      assertEquals(both.size, 2)
      assert(both.contains(first.head))

  test("the flat takes the best band of the observations using the mask"):
    for
      (pid, tid, aid, oid) <- gnMosWithMask()
      other                <- createObservationWithModeAs(pi, pid, List(tid), gmosNorthMosMode(aid.some))
      _                    <- setAllocationsAs(staff, pid, List(
                                AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan),
                                AllocationInput(TimeAccountingCategory.US, ScienceBand.Band3, 1.hourTimeSpan)
                              ))
      _                    <- setScienceBandAs(pi, oid, ScienceBand.Band3.some)
      _                    <- setScienceBandAs(pi, other, ScienceBand.Band2.some)
      _                    <- recalc(pid, oid)
      flats                <- flatsOf(pid)
      band                 <- queryScienceBand(flats.head)
    yield assertEquals(band, ScienceBand.Band2.some)

  test("an untaken flat follows edits to the mode it copied, a started one does not"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalc(pid, oid)
      flat1              <- flatsOf(pid)
      _                  <- setMask(oid, aid.some, "CUSTOM_WIDTH_1_00")
      _                  <- recalc(pid, oid)
      followed           <- slitWidthOf(flat1.head)
      _                  <- start(flat1.head)
      _                  <- setMask(oid, aid.some, "CUSTOM_WIDTH_0_75")
      _                  <- recalc(pid, oid)
      kept               <- slitWidthOf(flat1.head)
    yield
      assertEquals(followed, "CUSTOM_WIDTH_1_00")
      assertEquals(kept, "CUSTOM_WIDTH_1_00")

  test("GMOS North flat is one day-cal flat through the mask: mirror, r, 1x1, fast, low, full frame"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalc(pid, oid)
      flat             <- flatsOf(pid).map(_.head)
      _                <- runObscalcUpdate(pid, flat)
      c                <- query(pi, executionConfigQuery(flat, "gmosNorth", "science",
                            """
                              description
                              steps {
                                observeClass
                                stepConfig { stepType }
                                instrumentConfig {
                                  gratingConfig { grating }
                                  filter
                                  fpu { customMask { slitWidth } builtin }
                                  readout { xBin yBin ampReadMode ampGain }
                                  roi
                                }
                              }
                            """, 10.some))
      acq              <- query(pi, executionConfigQuery(flat, "gmosNorth", "acquisition", "observeClass", none))
    yield
      val sci   = c.hcursor.downFields("executionConfig", "gmosNorth", "science")
      val atom  = sci.downField("nextAtom")
      val step  = atom.downField("steps").downArray
      val inst  = step.downField("instrumentConfig")
      assertEquals(atom.downField("description").as[String].toOption, "MOS Mask Flat".some)
      assertEquals(sci.downField("possibleFuture").as[List[Json]].toOption, List.empty[Json].some)
      assertEquals(step.downField("observeClass").as[String].toOption, "DAY_CAL".some)
      assertEquals(step.downFields("stepConfig", "stepType").as[String].toOption, "GCAL".some)
      assert(inst.downField("gratingConfig").focus.exists(_.isNull))
      assertEquals(inst.downField("filter").as[String].toOption, "R_PRIME".some)
      assertEquals(inst.downFields("fpu", "customMask", "slitWidth").as[String].toOption, "CUSTOM_WIDTH_0_50".some)
      assertEquals(inst.downFields("readout", "xBin").as[String].toOption, "ONE".some)
      assertEquals(inst.downFields("readout", "yBin").as[String].toOption, "ONE".some)
      assertEquals(inst.downFields("readout", "ampReadMode").as[String].toOption, "FAST".some)
      assertEquals(inst.downFields("readout", "ampGain").as[String].toOption, "LOW".some)
      assertEquals(inst.downField("roi").as[String].toOption, "FULL_FRAME".some)
      assert(acq.hcursor.downFields("executionConfig", "gmosNorth", "acquisition", "nextAtom").focus.forall(_.isNull))

  test("GMOS South mask gets a flat with r"):
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      aid  <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.GmosSouth)
      oid  <- createObservationWithModeAs(pi, pid, List(tid), gmosSouthMosMode(aid.some))
      _    <- recalc(pid, oid)
      flat <- flatsOf(pid).map(_.head)
      _    <- runObscalcUpdate(pid, flat)
      c    <- query(pi, executionConfigQuery(flat, "gmosSouth", "science", "steps { instrumentConfig { filter gratingConfig { grating } } }", none))
    yield
      val inst = c.hcursor.downFields("executionConfig", "gmosSouth", "science", "nextAtom", "steps").downArray.downField("instrumentConfig")
      assertEquals(inst.downField("filter").as[String].toOption, "R_PRIME".some)
      assert(inst.downField("gratingConfig").focus.exists(_.isNull))

  test("Flamingos-2 flat is made, but its missing Smart GCAL row stops generation"):
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      aid  <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.Flamingos2)
      oid  <- createObservationWithModeAs(pi, pid, List(tid), flamingos2MosMode(aid.some))
      _    <- recalc(pid, oid)
      flat <- flatsOf(pid).map(_.head)
      mask <- maskOfFlat(flat, "flamingos2Mos")
      res  <- query(pi, executionConfigQuery(flat, "flamingos2", "science", "observeClass", none)).attempt
    yield
      assertEquals(mask, aid.some)
      assert(
        res.left.exists(_.getMessage.contains("No Smart GCAL imaging flat is defined for this configuration: Flamingos2 { disperser: None, filter: H, fpu: LongSlit2 }")),
        s"unexpected result: $res"
      )
