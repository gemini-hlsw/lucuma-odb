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
import lucuma.itc.IntegrationTime

/**
 * GMOS North IFU generation.
 *
 * The science sequence is the long slit sequence with the IFU aperture in place of the slit: the
 * same grating-dependent dithers and the same smart flats and arcs.  Only the readout (unbinned)
 * and the acquisition differ.
 */
class executionSciGmosNorthIfu extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  // The IFU is read out unbinned and the aperture itself is the focal plane unit.  The arc, flat
  // and science helpers all derive from this one.
  override def gmosNorthScience(ditherNm: Int): GmosNorth =
    super.gmosNorthScience(ditherNm).copy(
      readout = GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      fpu     = GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some
    )

  private def setup: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthIfuObservationAs(pi, p, List(t))
    yield o

  test("science - same shape as the long slit"):
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  // The IFU has its own sky field, so it does not nod: every step is on axis.
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 0, 0),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 0, 0)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  private def acqStep(fpu: Option[GmosFpuMask[GmosNorthFpu]], xBin: GmosXBinning, yBin: GmosYBinning, roi: GmosRoi): GmosNorth =
    gmosNorthScience(0).copy(
      exposure      = fakeItcImagingResult.exposureTime,
      readout       = GmosCcdMode(xBin, yBin, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      roi           = roi,
      gratingConfig = none,
      filter        = GmosNorthFilter.GPrime.some,
      fpu           = fpu
    )

  // The last step of the initial atom carries the breakpoint, placed by `AcquisitionAtoms`.
  private def expectedAcqStep(d: GmosNorth, breakpoint: Breakpoint): Json =
    json"""
      {
        "instrumentConfig" : ${gmosNorthExpectedInstrumentConfig(d)},
        "stepConfig" : { "stepType":  "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(0, 0, StepGuideState.Enabled)},
        "observeClass" : "ACQUISITION",
        "breakpoint": ${breakpoint.tag.toScreamingSnakeCase.asJson}
      }
    """

  test("acquisition - CCD2 field image then a Full Frame image through the IFU"):
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
                    "steps"        -> List(
                      expectedAcqStep(acqStep(none, GmosXBinning.Two, GmosYBinning.Two, GmosRoi.Ccd2), Breakpoint.Disabled),
                      expectedAcqStep(acqStep(GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits).some, GmosXBinning.One, GmosYBinning.One, GmosRoi.FullFrame), Breakpoint.Enabled)
                    ).asJson
                  ),
                  "possibleFuture" -> Json.arr(),
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )
