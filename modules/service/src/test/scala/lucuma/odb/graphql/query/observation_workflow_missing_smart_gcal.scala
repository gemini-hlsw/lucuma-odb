// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.graphql.mutation.ReplaceGmosNorthSequenceOps
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.SmartGcalValue
import skunk.Session

// sc-11465: a calibration without a Smart GCAL definition is left out of the
// sequence and reported, instead of failing the whole sequence.
class observation_workflow_missing_smart_gcal extends ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  private val AllWavelengths: BoundedInterval[Wavelength] =
    BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)

  private def key(filter: Option[GmosNorthFilter]): Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(GmosNorthGrating.R600_G5304, GmosGratingOrder.One, AllWavelengths).some,
      filter,
      GmosNorthFpu.LongSlit_0_50.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  private val noCalibration: SmartGcalValue.Legacy => SmartGcalValue.Legacy =
    SmartGcalValue.stepCount.replace(NonNegInt.unsafeFrom(0))

  // No filter: nothing defined.  GG455: an arc alone.  OG515: flat and arc
  // marked as having no calibration.  RG610: flat and arc.
  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val rows: List[Gmos.TableRow.North] =
      List(
        Gmos.TableRow(PosLong.unsafeFrom(1), key(GmosNorthFilter.GG455.some), gmos_arc),
        Gmos.TableRow(PosLong.unsafeFrom(1), key(GmosNorthFilter.OG515.some), noCalibration(gmos_flat)),
        Gmos.TableRow(PosLong.unsafeFrom(1), key(GmosNorthFilter.OG515.some), noCalibration(gmos_arc)),
        Gmos.TableRow(PosLong.unsafeFrom(1), key(GmosNorthFilter.RG610.some), gmos_flat),
        Gmos.TableRow(PosLong.unsafeFrom(1), key(GmosNorthFilter.RG610.some), gmos_arc)
      )

    val extra = servicesFor(pi).map(_(s)).use: services =>
      services.transactionally:
        Services.asSuperUser:
          rows.zipWithIndex.traverse_((r, i) => services.smartGcalService.insertGmosNorth(100 + i, r))

    super.dbInitialization.fold(IO.unit)(_(s)) >> extra
  }

  private def mode(filter: Option[String]): String =
    s"""
      gmosNorthLongSlit: {
        grating: R600_G5304
        ${filter.foldMap(f => s"filter: $f")}
        fpu: LONG_SLIT_0_50 explicitXBin: ONE explicitYBin: TWO
        centralWavelength: { nanometers: 500 }
        explicitWavelengthDithers: [ { nanometers: 0 } ]
      }
    """

  private def createObservation(filter: Option[String]): IO[(Program.Id, Observation.Id)] =
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), mode(filter))
      _   <- runObscalcUpdate(pid, oid)
    yield (pid, oid)

  private def missing(oid: Observation.Id): IO[List[String]] =
    queryObservationValidationsAs(pi, oid).map:
      _.collect { case (ObservationValidationCode.MissingSmartGcal, msgs) => msgs }.flatten

  // The kinds of step in the first science atom: SCIENCE, FLAT or ARC.
  private def firstAtom(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          executionConfig(observationId: "$oid") {
            gmosNorth {
              science {
                nextAtom {
                  steps {
                    stepConfig {
                      stepType
                      ... on Gcal { arcs }
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json.hcursor
        .downFields("executionConfig", "gmosNorth", "science", "nextAtom", "steps")
        .values
        .toList
        .flatten
        .map: step =>
          val c = step.hcursor.downField("stepConfig")
          c.get[String]("stepType").toOption match
            case Some("GCAL") => if c.get[List[String]]("arcs").exists(_.nonEmpty) then "ARC" else "FLAT"
            case other        => other.orEmpty

  private def message(calibration: String): String =
    s"Science sequence: No Smart GCAL $calibration is defined for this configuration: GmosNorth {"

  test("nothing defined: the science steps are generated and both calibrations reported"):
    for
      (_, oid) <- createObservation(none)
      steps    <- firstAtom(oid)
      msgs     <- missing(oid)
    yield
      assertEquals(steps.distinct, List("SCIENCE"))
      assertEquals(msgs.size, 2, msgs.toString)
      assert(msgs.exists(_.startsWith(message("spectroscopic flat"))), msgs.toString)
      assert(msgs.exists(_.startsWith(message("arc"))), msgs.toString)
      assert(msgs.forall(_.contains("R600_G5304")), msgs.toString)

  test("each calibration is looked up on its own: a missing flat leaves the arc"):
    for
      (_, oid) <- createObservation("GG455".some)
      steps    <- firstAtom(oid)
      msgs     <- missing(oid)
    yield
      assertEquals(steps.distinct.sorted, List("ARC", "SCIENCE"))
      assertEquals(msgs.size, 1, msgs.toString)
      assert(msgs.forall(_.startsWith(message("spectroscopic flat"))), msgs.toString)

  test("a no-calibration row yields no steps and no warning"):
    for
      (_, oid) <- createObservation("OG515".some)
      steps    <- firstAtom(oid)
      msgs     <- missing(oid)
    yield
      assertEquals(steps.distinct, List("SCIENCE"))
      assertEquals(msgs, Nil)

  test("defined calibrations are generated without a warning"):
    for
      (_, oid) <- createObservation("RG610".some)
      steps    <- firstAtom(oid)
      msgs     <- missing(oid)
    yield
      assertEquals(steps.distinct.sorted, List("ARC", "FLAT", "SCIENCE"))
      assertEquals(msgs, Nil)

  test("an engineering program, which has no proposal, is warned too"):
    for
      pid  <- createProgram
      _    <- setProgramReference(staff, pid, """engineering: { semester: "2025B", instrument: GMOS_NORTH }""")
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createObservationWithModeAs(pi, pid, List(tid), mode(none))
      _    <- runObscalcUpdate(pid, oid)
      msgs <- missing(oid)
    yield assertEquals(msgs.size, 2, msgs.toString)

  test("a stored sequence runs no lookup and reports nothing"):
    val science =
      """
        {
          instrumentConfig: {
            exposure: { seconds: 20 }
            readout: { xBin: ONE, yBin: TWO, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
            dtax: ZERO
            roi: FULL_FRAME
            gratingConfig: { grating: R600_G5304, order: ONE, wavelength: { nanometers: 500 } }
            fpu: { builtin: LONG_SLIT_0_50 }
          }
          stepConfig: { science: true }
          observeClass: SCIENCE
        }
      """
    for
      (pid, oid) <- createObservation(none)
      before     <- missing(oid)
      _          <- query(pi, mutation(Instrument.GmosNorth, input(oid, SequenceType.Science, atomInput("Edited", science))))
      _          <- runObscalcUpdate(pid, oid)
      after      <- missing(oid)
    yield
      assertEquals(before.size, 2, before.toString)
      assertEquals(after, Nil)
