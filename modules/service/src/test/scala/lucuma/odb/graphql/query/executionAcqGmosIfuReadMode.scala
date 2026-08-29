// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
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
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

/**
 * The acquisition reads out at one mode throughout, taken from the field image's exposure, as the
 * long slit does.  30s is the case that tells the two candidate behaviours apart: the field image
 * is under the 60s Fast limit but the step through the IFU, at 4x, is over it.  Deriving the mode
 * per step would make that second step Slow.
 */
class executionAcqGmosIfuReadMode extends ExecutionTestSupportForGmos:

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(30.secTimeSpan, PosInt.unsafeFrom(1))

  override def gmosNorthScience(ditherNm: Int): GmosNorth =
    super.gmosNorthScience(ditherNm).copy(
      readout = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      fpu     = GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some
    )

  private val fieldStep: GmosNorth =
    gmosNorthScience(0).copy(
      exposure      = 30.secTimeSpan,
      readout       = GmosCcdMode(GmosXBinning.Two, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi           = GmosRoi.Ccd2,
      gratingConfig = none,
      filter        = GmosNorthFilter.GPrime.some,
      fpu           = none
    )

  private val ifuStep: GmosNorth =
    fieldStep.copy(
      exposure = 120.secTimeSpan,
      readout  = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi      = GmosRoi.FullFrame,
      fpu      = GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some
    )

  private def expectedStep(d: GmosNorth): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(d)},
        "stepConfig" : { "stepType":  "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(0, 0, StepGuideState.Enabled)},
        "observeClass" : "ACQUISITION",
        "breakpoint": "ENABLED"
      }
    """

  test("both steps read out Fast, even though the IFU step is over the 60s limit"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthIfuObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
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
                    "steps"        -> List(expectedStep(fieldStep), expectedStep(ifuStep)).asJson
                  ),
                  "possibleFuture" -> Json.arr(),
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )
