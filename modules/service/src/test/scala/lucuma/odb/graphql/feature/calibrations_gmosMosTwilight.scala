// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.ACursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.math.Angle
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.PerProgramPerConfigCalibrationsService
import lucuma.odb.util.Codecs.attachment_id
import skunk.codec.all.text
import skunk.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class calibrations_gmosMosTwilight
  extends ExecutionTestSupportForGmos
  with TelluricCalibrationsTestSupport
  with MosMaskSupport:

  val when: Instant = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  private val DesignAngle: Angle = Angle.fromDoubleDegrees(37.0)

  private val Readout: String =
    "explicitXBin: ONE, explicitYBin: ONE, explicitAmpReadMode: SLOW, explicitAmpGain: LOW"

  private def gnMos(aid: Option[Attachment.Id], cw: Int = 500): String =
    s"""
      gmosNorthMos: {
        grating: R831_G5302
        filter: R_PRIME
        customMask: { slitWidth: CUSTOM_WIDTH_0_50${aid.fold("")(a => s", attachmentId: ${a.asJson}")} }
        centralWavelength: { nanometers: $cw }
        $Readout
      }
    """

  private val gnLongSlit: String =
    s"""
      gmosNorthLongSlit: {
        grating: R831_G5302
        filter: R_PRIME
        fpu: LONG_SLIT_0_50
        centralWavelength: { nanometers: 500 }
        $Readout
      }
    """

  private def setMask(oid: Observation.Id, aid: Option[Attachment.Id]): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { observingMode: { gmosNorthMos: { customMask: { slitWidth: CUSTOM_WIDTH_0_50, attachmentId: ${aid.fold("null")(_.asJson.noSpaces)} } } } }
            }) {
              observations { id }
            }
          }"""
    ).void

  private def replaceFile(aid: Attachment.Id, path: String): IO[Unit] =
    withSession(_.execute(sql"UPDATE t_attachment SET c_remote_path = $text WHERE c_attachment_id = $attachment_id".command)((path, aid)).void)

  private def deleteAttachmentRow(aid: Attachment.Id): IO[Unit] =
    withSession(_.execute(sql"DELETE FROM t_attachment WHERE c_attachment_id = $attachment_id".command)(aid).void)

  private def setScienceSnAt(oid: Observation.Id, nm: Int): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: { observingMode: { gmosNorthMos: { exposureTimeMode: { signalToNoise: { value: 75, at: { nanometers: $nm } } } } } }
            }) {
              observations { id }
            }
          }"""
    ).void

  private def twilightSnAt(oid: Observation.Id): IO[Option[Int]] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              observingMode { gmosNorthMos { exposureTimeMode { signalToNoise { at { nanometers } } } } }
            }
          }"""
    ).map(_.hcursor.downFields("observation", "observingMode", "gmosNorthMos", "exposureTimeMode", "signalToNoise", "at", "nanometers").as[BigDecimal].toOption.map(_.toInt))

  private def start(oid: Observation.Id): IO[Unit] =
    recordVisitAs(serviceUser, oid).flatMap(addSequenceEventAs(serviceUser, _, SequenceCommand.Start)).void

  private def twilightsOf(pid: Program.Id): IO[List[Observation.Id]] =
    queryCalibrationsWithRole(pid, CalibrationRole.Twilight)

  private def specPhotsOf(pid: Program.Id): IO[List[Observation.Id]] =
    queryCalibrationsWithRole(pid, CalibrationRole.SpectroPhotometric)

  private def observation(oid: Observation.Id): IO[ACursor] =
    query(
      serviceUser,
      s"""query {
            observation(observationId: "$oid") {
              posAngleConstraint { mode angle { microarcseconds } }
              observingMode {
                mode
                gmosNorthMos {
                  customMask { attachmentId }
                  centralWavelength { nanometers }
                  telescopeConfigs { offset { q { arcseconds } } guiding }
                }
                gmosSouthMos { customMask { attachmentId } }
                gmosNorthLongSlit { fpu }
                gmosSouthLongSlit { fpu }
              }
            }
          }"""
    ).map(_.hcursor.downField("observation"))

  private def modeOf(oid: Observation.Id): IO[String] =
    observation(oid).flatMap(_.downFields("observingMode", "mode").as[String].liftTo[IO])

  private def maskOf(oid: Observation.Id, mode: String = "gmosNorthMos"): IO[Option[Attachment.Id]] =
    observation(oid).map(_.downFields("observingMode", mode, "customMask", "attachmentId").as[Option[Attachment.Id]].toOption.flatten)

  private def gnMosWithMask(cw: Int = 500): IO[(Program.Id, Target.Id, Attachment.Id, Observation.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      aid <- insertMosMaskAttachment(pid, "GN2025AQ001-01_ODF.fits", Instrument.GmosNorth, DesignAngle.some)
      oid <- createObservationWithModeAs(pi, pid, List(tid), gnMos(aid.some, cw))
    yield (pid, tid, aid, oid)

  test("MOS science with a Mask Attachment gets a MOS Twilight through its Custom Mask in the Calibrations group"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalculateCalibrations(pid, when, oid)
      twilights          <- twilightsOf(pid)
      mode               <- modeOf(twilights.head)
      mask               <- maskOf(twilights.head)
      gid                <- queryObservation(twilights.head).map(_.groupId)
      group              <- gid.traverse(queryGroupNameAndRoles)
    yield
      assertEquals(twilights.size, 1)
      assertEquals(mode, "GMOS_NORTH_MOS")
      assertEquals(mask, aid.some)
      assertEquals(group.map(_.name), PerProgramPerConfigCalibrationsService.CalibrationsGroupName.value.some)

  test("the MOS Twilight is fixed at the Mask Design Angle with default offsets"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      twilight         <- twilightsOf(pid).map(_.head)
      obs              <- observation(twilight)
    yield
      val pac = obs.downField("posAngleConstraint")
      assertEquals(pac.downField("mode").as[String].toOption, "FIXED".some)
      assertEquals(pac.downFields("angle", "microarcseconds").as[Long].toOption, DesignAngle.toMicroarcseconds.some)
      val tcs = obs.downFields("observingMode", "gmosNorthMos", "telescopeConfigs").as[List[Json]].toOption
      assertEquals(tcs.map(_.size), 1.some)

  test("MOS science gets a long slit specphot at the equivalent slit width"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      specPhots        <- specPhotsOf(pid)
      obs              <- observation(specPhots.head)
    yield
      assertEquals(specPhots.size, 1)
      assertEquals(obs.downFields("observingMode", "mode").as[String].toOption, "GMOS_NORTH_LONG_SLIT".some)
      assertEquals(obs.downFields("observingMode", "gmosNorthLongSlit", "fpu").as[String].toOption, "LONG_SLIT_0_50".some)

  test("MOS science without a Mask Attachment gets a specphot but no Twilight"):
    for
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createObservationWithModeAs(pi, pid, List(tid), gnMos(none))
      _         <- recalculateCalibrations(pid, when, oid)
      twilights <- twilightsOf(pid)
      specPhots <- specPhotsOf(pid)
    yield
      assertEquals(twilights, Nil)
      assertEquals(specPhots.size, 1)

  test("MOS and long slit science of one setup share the specphot but not the Twilight"):
    for
      (pid, tid, _, oid) <- gnMosWithMask()
      _                  <- createObservationWithModeAs(pi, pid, List(tid), gnLongSlit)
      _                  <- recalculateCalibrations(pid, when, oid)
      twilights          <- twilightsOf(pid)
      modes              <- twilights.traverse(modeOf)
      specPhots          <- specPhotsOf(pid)
    yield
      assertEquals(modes.sorted, List("GMOS_NORTH_LONG_SLIT", "GMOS_NORTH_MOS"))
      assertEquals(specPhots.size, 1)

  test("science sharing a Mask Attachment and setup shares a Twilight; another wavelength or attachment gets its own"):
    for
      (pid, tid, aid, oid) <- gnMosWithMask()
      _                    <- createObservationWithModeAs(pi, pid, List(tid), gnMos(aid.some))
      _                    <- recalculateCalibrations(pid, when, oid)
      shared               <- twilightsOf(pid)
      _                    <- createObservationWithModeAs(pi, pid, List(tid), gnMos(aid.some, 600))
      other                <- insertMosMaskAttachment(pid, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth, DesignAngle.some)
      _                    <- createObservationWithModeAs(pi, pid, List(tid), gnMos(other.some))
      _                    <- recalculateCalibrations(pid, when, oid)
      all                  <- twilightsOf(pid)
      masks                <- all.traverse(maskOf(_))
    yield
      assertEquals(shared.size, 1)
      assertEquals(all.size, 3)
      assertEquals(masks.flatten.toSet, Set(aid, other))

  test("a second calibration pass keeps the MOS Twilight"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      before           <- twilightsOf(pid)
      (added, removed) <- recalculateCalibrations(pid, when, oid)
      after            <- twilightsOf(pid)
    yield
      assertEquals(after, before)
      assertEquals(added, Nil)
      assertEquals(removed, Nil)

  test("unassigning the Mask Attachment removes an untaken MOS Twilight"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      before           <- twilightsOf(pid)
      _                <- setMask(oid, none)
      _                <- recalculateCalibrations(pid, when, oid)
      after            <- twilightsOf(pid)
    yield
      assertEquals(before.size, 1)
      assertEquals(after, Nil)

  test("deleting the Mask Attachment removes an untaken MOS Twilight"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalculateCalibrations(pid, when, oid)
      before             <- twilightsOf(pid)
      _                  <- deleteAttachmentRow(aid)
      _                  <- recalculateCalibrations(pid, when, oid)
      after              <- twilightsOf(pid)
    yield
      assertEquals(before.size, 1)
      assertEquals(after, Nil)

  test("replacing the attachment's file keeps an untaken MOS Twilight"):
    for
      (pid, _, aid, oid) <- gnMosWithMask()
      _                  <- recalculateCalibrations(pid, when, oid)
      first              <- twilightsOf(pid)
      _                  <- replaceFile(aid, "second")
      (added, removed)   <- recalculateCalibrations(pid, when, oid)
      after              <- twilightsOf(pid)
    yield
      assertEquals(after, first)
      assertEquals(added, Nil)
      assertEquals(removed, Nil)

  test("a started MOS Twilight is kept and a new one is made beside it"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      first            <- twilightsOf(pid)
      _                <- start(first.head)
      _                <- recalculateCalibrations(pid, when, oid)
      both             <- twilightsOf(pid)
    yield
      assertEquals(both.size, 2)
      assert(both.contains(first.head))

  test("the MOS Twilight's science S/N wavelength follows its science"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- setScienceSnAt(oid, 520)
      _                <- recalculateCalibrations(pid, when, oid)
      twilight         <- twilightsOf(pid).map(_.head)
      _                <- recalculateCalibrations(pid, when, oid)
      before           <- twilightSnAt(twilight)
      _                <- setScienceSnAt(oid, 540)
      _                <- recalculateCalibrations(pid, when, oid)
      after            <- twilightSnAt(twilight)
    yield
      assertEquals(before, 520.some)
      assertEquals(after, 540.some)

  test("GMOS South MOS science gets a MOS Twilight through its Custom Mask"):
    for
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      aid       <- insertMosMaskAttachment(pid, "GS2025AQ001-01_ODF.fits", Instrument.GmosSouth, DesignAngle.some)
      oid       <- createObservationWithModeAs(pi, pid, List(tid), gmosSouthMosMode(aid.some))
      _         <- recalculateCalibrations(pid, when, oid)
      twilights <- twilightsOf(pid)
      mode      <- modeOf(twilights.head)
      mask      <- maskOf(twilights.head, "gmosSouthMos")
    yield
      assertEquals(twilights.size, 1)
      assertEquals(mode, "GMOS_SOUTH_MOS")
      assertEquals(mask, aid.some)

  test("the MOS Twilight sequence observes through the Custom Mask, unguided, with no acquisition"):
    for
      (pid, _, _, oid) <- gnMosWithMask()
      _                <- recalculateCalibrations(pid, when, oid)
      twilight         <- twilightsOf(pid).map(_.head)
      _                <- runObscalcUpdate(pid, twilight)
      c                <- query(pi, executionConfigQuery(twilight, "gmosNorth", "science",
                            """
                              steps {
                                observeClass
                                stepConfig { stepType }
                                telescopeConfig { guiding }
                                instrumentConfig { fpu { customMask { slitWidth } } }
                              }
                            """, none))
      acq              <- query(pi, executionConfigQuery(twilight, "gmosNorth", "acquisition", "observeClass", none))
    yield
      val step = c.hcursor.downFields("executionConfig", "gmosNorth", "science", "nextAtom", "steps").downArray
      assertEquals(step.downField("observeClass").as[String].toOption, "DAY_CAL".some)
      assertEquals(step.downFields("stepConfig", "stepType").as[String].toOption, "SCIENCE".some)
      assertEquals(step.downFields("telescopeConfig", "guiding").as[String].toOption, "DISABLED".some)
      assertEquals(step.downFields("instrumentConfig", "fpu", "customMask", "slitWidth").as[String].toOption, "CUSTOM_WIDTH_0_50".some)
      assert(acq.hcursor.downFields("executionConfig", "gmosNorth", "acquisition", "nextAtom").focus.forall(_.isNull))

  test("an existing MOS Twilight follows the best band of its science"):
    for
      (pid, tid, aid, oid) <- gnMosWithMask()
      other                <- createObservationWithModeAs(pi, pid, List(tid), gnMos(aid.some))
      _                    <- setAllocationsAs(staff, pid, List(
                                AllocationInput(TimeAccountingCategory.US, ScienceBand.Band2, 1.hourTimeSpan),
                                AllocationInput(TimeAccountingCategory.US, ScienceBand.Band3, 1.hourTimeSpan)
                              ))
      _                    <- setScienceBandAs(pi, oid, ScienceBand.Band3.some)
      _                    <- setScienceBandAs(pi, other, ScienceBand.Band3.some)
      _                    <- recalculateCalibrations(pid, when, oid)
      twilight             <- twilightsOf(pid).map(_.head)
      before               <- queryScienceBand(twilight)
      _                    <- setScienceBandAs(pi, other, ScienceBand.Band2.some)
      _                    <- recalculateCalibrations(pid, when, oid)
      after                <- queryScienceBand(twilight)
    yield
      assertEquals(before, ScienceBand.Band3.some)
      assertEquals(after, ScienceBand.Band2.some)
