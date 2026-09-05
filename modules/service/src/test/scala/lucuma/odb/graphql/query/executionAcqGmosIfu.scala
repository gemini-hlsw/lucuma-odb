// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

/**
 * The two GMOS IFU acquisition parameters that the main suite cannot reach: the cap on the
 * through-IFU exposure, which needs a long ITC acquisition time, and the acquisition ROI, which
 * pairs the field image's ROI with the one used through the IFU.
 */
class executionAcqGmosIfu extends ExecutionTestSupportForGmos:

  // Above the GMOS-wide 180s acquisition clamp, so the field image comes back at 180s; 4 x 180s
  // then exceeds the through-IFU step's four minute cap.
  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(300.secTimeSpan, PosInt.unsafeFrom(1))

  override def gmosNorthScience(ditherNm: Int): GmosNorth =
    super.gmosNorthScience(ditherNm).copy(
      readout = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      fpu     = GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some
    )

  private def fieldStep(roi: GmosRoi): GmosNorth =
    gmosNorthScience(0).copy(
      exposure      = 180.secTimeSpan,
      readout       = GmosCcdMode(GmosXBinning.Two, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      roi           = roi,
      gratingConfig = none,
      filter        = GmosNorthFilter.GPrime.some,
      fpu           = none
    )

  private def ifuStep(roi: GmosRoi, exposure: TimeSpan, readMode: GmosAmpReadMode): GmosNorth =
    fieldStep(roi).copy(
      exposure = exposure,
      readout  = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, readMode),
      fpu      = GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some
    )

  private def expectedStep(d: GmosNorth): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(d)},
        "stepConfig" : { "stepType":  "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(0, 0, StepGuideState.Enabled)},
        "observeClass" : "ACQUISITION",
        "breakpoint": ${Breakpoint.Enabled.tag.toScreamingSnakeCase.asJson}
      }
    """

  private def expectInitialAtom(oid: Observation.Id, steps: List[Json]): IO[Unit] =
    expect(
      user     = pi,
      query    = gmosNorthAcquisitionQuery(oid, 0.some),
      expected =
        Json.obj(
          "executionConfig" -> Json.obj(
            "gmosNorth" -> Json.obj(
              "acquisition" -> Json.obj(
                "nextAtom" -> Json.obj(
                  "description"  -> "Initial Acquisition".asJson,
                  "observeClass" -> "ACQUISITION".asJson,
                  "steps"        -> steps.asJson
                ),
                "possibleFuture" -> Json.arr(),
                "hasMore" -> true.asJson
              )
            )
          )
        ).asRight
    )

  private def mode(acquisition: String): String =
    s"""
      gmosNorthIfu: {
        grating: R831_G5302
        filter: R_PRIME
        fpu: TWO_SLITS
        centralWavelength: { nanometers: 500 }
        $acquisition
      }
    """

  private def setup(acquisition: String): IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(acquisition))
    yield o

  test("the field image takes the GMOS acquisition clamp and the IFU step its own four minute cap"):
    setup("").flatMap: oid =>
      expectInitialAtom(oid, List(
        expectedStep(fieldStep(GmosRoi.Ccd2)),
        expectedStep(ifuStep(GmosRoi.FullFrame, 240.secTimeSpan, GmosAmpReadMode.Slow))
      ))

  test("an explicit acquisition ROI drives both steps"):
    setup("acquisition: { explicitRoi: STAMP_FULL_FRAME }").flatMap: oid =>
      expectInitialAtom(oid, List(
        expectedStep(fieldStep(GmosRoi.CentralStamp)),
        expectedStep(ifuStep(GmosRoi.FullFrame, 240.secTimeSpan, GmosAmpReadMode.Slow))
      ))

  test("a Full Frame acquisition ROI uses it for the field image too"):
    setup("acquisition: { explicitRoi: FULL_FRAME }").flatMap: oid =>
      expectInitialAtom(oid, List(
        expectedStep(fieldStep(GmosRoi.FullFrame)),
        expectedStep(ifuStep(GmosRoi.FullFrame, 240.secTimeSpan, GmosAmpReadMode.Slow))
      ))

  // `AcquisitionAtoms` only breaks after the initial atom's last step, so the repeating atom's
  // breakpoint has to come from the steps themselves.
  test("the repeating atom's step carries a breakpoint too"):
    setup("").flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "acquisition" -> Json.obj(
                  "nextAtom" -> Json.obj(
                    "description"  -> "Initial Acquisition".asJson,
                    "observeClass" -> "ACQUISITION".asJson,
                    "steps"        -> List(
                      expectedStep(fieldStep(GmosRoi.Ccd2)),
                      expectedStep(ifuStep(GmosRoi.FullFrame, 240.secTimeSpan, GmosAmpReadMode.Slow))
                    ).asJson
                  ),
                  "possibleFuture" -> List(
                    Json.obj(
                      "description"  -> "Fine Adjustments".asJson,
                      "observeClass" -> "ACQUISITION".asJson,
                      "steps"        -> List(
                        expectedStep(ifuStep(GmosRoi.FullFrame, 240.secTimeSpan, GmosAmpReadMode.Slow))
                      ).asJson
                    )
                  ).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )
