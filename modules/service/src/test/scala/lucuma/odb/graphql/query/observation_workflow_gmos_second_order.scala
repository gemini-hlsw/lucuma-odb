// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.SequenceType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.mutation.ReplaceGmosNorthSequenceOps
import lucuma.odb.service.Services
import lucuma.odb.service.workflow.validator.GmosSecondOrderOverlapValidator
import lucuma.odb.smartgcal.data.Gmos
import skunk.Session

class observation_workflow_gmos_second_order extends ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  private val AllWavelengths: BoundedInterval[Wavelength] =
    BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)

  // The overlap is found in the generated sequence, which needs a Smart GCAL
  // mapping for each configuration used below.
  private def northKey(filter: Option[GmosNorthFilter]): Gmos.TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(GmosNorthGrating.R400_G5310, GmosGratingOrder.One, AllWavelengths).some,
      filter,
      GmosNorthFpu.LongSlit_0_50.some,
      GmosXBinning.One,
      GmosYBinning.Two,
      GmosAmpGain.Low
    )

  private val southIfuKey: Gmos.TableKey[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
    Gmos.TableKey(
      Gmos.GratingConfigKey(GmosSouthGrating.R400_G5325, GmosGratingOrder.One, AllWavelengths).some,
      none,
      GmosSouthFpu.Ifu2Slits.some,
      GmosXBinning.One,
      GmosYBinning.One,
      GmosAmpGain.Low
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val north: List[Gmos.TableRow.North] =
      List(none, GmosNorthFilter.GG455.some, GmosNorthFilter.OG515.some).flatMap: f =>
        List(gmos_flat, gmos_arc).map(v => Gmos.TableRow(PosLong.unsafeFrom(1), northKey(f), v))

    val south: List[Gmos.TableRow.South] =
      List(gmos_flat, gmos_arc).map(v => Gmos.TableRow(PosLong.unsafeFrom(1), southIfuKey, v))

    val extra = servicesFor(pi).map(_(s)).use: services =>
      services.transactionally:
        Services.asSuperUser:
          north.zipWithIndex.traverse_((r, i) => services.smartGcalService.insertGmosNorth(100 + i, r)) >>
          south.zipWithIndex.traverse_((r, i) => services.smartGcalService.insertGmosSouth(100 + i, r))

    super.dbInitialization.fold(IO.unit)(_(s)) >> extra
  }

  private def validations(oid: Observation.Id): IO[List[(ObservationValidationCode, List[String])]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow {
              value {
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("observation", "workflow", "value", "validationErrors")
        .values
        .toList
        .flatten
        .map: v =>
          (
            v.hcursor.downField("code").require[ObservationValidationCode],
            v.hcursor.downField("messages").require[List[String]]
          )

  private def secondOrderValidations(oid: Observation.Id): IO[List[List[String]]] =
    validations(oid).map(_.collect { case (ObservationValidationCode.SecondOrderOverlap, msgs) => msgs })

  private def secondOrderValidations(create: (Program.Id, Target.Id) => IO[Observation.Id]): IO[List[List[String]]] =
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- create(pid, tid)
      _   <- runObscalcUpdate(pid, oid)
      vs  <- secondOrderValidations(oid)
    yield vs

  // A science step with the given grating and filter, centered at `nm`.
  private def scienceStep(grating: String, filter: Option[String], nm: Int): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 20 }
          readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: SLOW }
          dtax: ZERO
          roi: FULL_FRAME
          gratingConfig: { grating: $grating, order: ONE, wavelength: { nanometers: $nm } }
          ${filter.foldMap(f => s"filter: $f")}
          fpu: { builtin: LONG_SLIT_0_50 }
        }
        stepConfig: { science: true }
        observeClass: SCIENCE
      }
    """

  // Replaces the science sequence with the given steps, then recalculates.
  private def editScience(pid: Program.Id, oid: Observation.Id, steps: String*): IO[Unit] =
    query(pi, mutation(Instrument.GmosNorth, input(oid, SequenceType.Science, atomInput("Edited", steps*)))) >>
    runObscalcUpdate(pid, oid)

  private def withMode(mode: String): (Program.Id, Target.Id) => IO[Observation.Id] =
    (pid, tid) => createObservationWithModeAs(pi, pid, List(tid), mode)

  private def nm(n: Int): Wavelength =
    Wavelength.unsafeFromIntPicometers(n * 1000)

  private def expected(onset: Int, affected: Int*): List[List[String]] =
    List(List(GmosSecondOrderOverlapValidator.message(nm(onset), NonEmptyList.fromListUnsafe(affected.toList.map(nm)))))

  test("GMOS North long slit without a filter, reaching past 720 nm"):
    // R400 covers 472 nm, dithered by 0, +8 and -8 nm by default
    assertIO(
      secondOrderValidations(withMode(
        """
          gmosNorthLongSlit: {
            grating: R400_G5310
            fpu: LONG_SLIT_0_50 explicitXBin: ONE explicitYBin: TWO
            centralWavelength: { nanometers: 700 }
          }
        """
      )),
      expected(720, 692, 700, 708)
    )

  test("GMOS North long slit, only the dithers reaching past the onset"):
    assertIO(
      secondOrderValidations(withMode(
        """
          gmosNorthLongSlit: {
            grating: R400_G5310
            fpu: LONG_SLIT_0_50 explicitXBin: ONE explicitYBin: TWO
            centralWavelength: { nanometers: 480 }
            explicitWavelengthDithers: [ { nanometers: 0 }, { nanometers: 5 }, { nanometers: 10 } ]
          }
        """
      )),
      expected(720, 485, 490)
    )

  test("GMOS North long slit with a long pass filter, reaching past twice its cut-on"):
    assertIO(
      secondOrderValidations(withMode(
        """
          gmosNorthLongSlit: {
            grating: R400_G5310
            filter: GG455
            fpu: LONG_SLIT_0_50 explicitXBin: ONE explicitYBin: TWO
            centralWavelength: { nanometers: 800 }
          }
        """
      )),
      expected(920, 792, 800, 808)
    )

  test("GMOS North long slit with a band pass filter ending before the onset"):
    assertIO(
      secondOrderValidations((pid, tid) => createGmosNorthLongSlitObservationAs(pi, pid, List(tid))),
      Nil
    )

  test("GMOS South IFU without a filter, reaching past 720 nm"):
    assertIO(
      secondOrderValidations(withMode(
        """
          gmosSouthIfu: {
            grating: R400_G5325
            fpu: TWO_SLITS
            centralWavelength: { nanometers: 700 }
          }
        """
      )),
      expected(720, 692, 700, 708)
    )

  test("an edited sequence that introduces overlap is warned"):
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(secondOrderValidations(oid), Nil)
      _   <- editScience(pid, oid, scienceStep("R400_G5310", None, 700), scienceStep("R831_G5302", "R_PRIME".some, 500))
      _   <- assertIO(secondOrderValidations(oid), expected(720, 700))
    yield ()

  test("an edited sequence that removes overlap is not warned"):
    val mode =
      """
        gmosNorthLongSlit: {
          grating: R400_G5310
          fpu: LONG_SLIT_0_50 explicitXBin: ONE explicitYBin: TWO
          centralWavelength: { nanometers: 700 }
        }
      """
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), mode)
      _   <- runObscalcUpdate(pid, oid)
      _   <- assertIO(secondOrderValidations(oid), expected(720, 692, 700, 708))
      // OG515 overlap starts at 1040 nm, past the 936 nm coverage at 700 nm
      _   <- editScience(pid, oid, scienceStep("R400_G5310", "OG515".some, 700))
      _   <- assertIO(secondOrderValidations(oid), Nil)
    yield ()
