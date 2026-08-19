// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.ACursorOps
import lucuma.odb.json.time.decoder.given
import lucuma.odb.sequence.flamingos2.mos.Acquisition.RepeatingAtomCount

/**
 * The Flamingos 2 MOS sequences: the equivalent long slit science sequence with the
 * custom mask on every step, and an acquisition that images the field with the mask
 * out of the beam before confirming the alignment through it.
 */
class executionSciFlamingos2Mos extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 5.minuteTimeSpan

  val AcqExposureTime: TimeSpan = 30.secTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(AcqExposureTime, PosInt.unsafeFrom(1))

  // CUSTOM_WIDTH_1_PIX is equivalent to LONG_SLIT_1, which is what f2_key_JH1 is keyed on.
  val SlitWidth: Flamingos2CustomSlitWidth =
    Flamingos2CustomSlitWidth.CustomWidth_1_pix

  val CustomMask: Flamingos2FpuMask =
    Flamingos2FpuMask.Custom(ToBeDefined, SlitWidth)

  private def mosScienceQuery(oid: Observation.Id): String =
    flamingos2ScienceQuery(oid, atomQuery = Flamingos2MosAtomQuery)

  private def mosAcquisitionQuery(oid: Observation.Id): String =
    flamingos2AcquisitionQuery(oid, atomQuery = Flamingos2MosAtomQuery)

  private def asMos(f2: Flamingos2DynamicConfig): Flamingos2DynamicConfig =
    f2.copy(fpu = CustomMask, decker = Flamingos2Decker.MOS)

  private def mosStep(f2: Flamingos2DynamicConfig, stepConfig: Json, tc: TelescopeConfig, obsClass: String): Json =
    json"""
      {
        "instrumentConfig": ${flamingos2ExpectedInstrumentConfig(asMos(f2))},
        "stepConfig": $stepConfig,
        "telescopeConfig": ${expectedTelescopeConfig(tc)},
        "observeClass": ${obsClass.asJson},
        "breakpoint": "DISABLED"
      }
    """

  // The acquisition steps: the mask out of the beam, then through it.
  private val MosAcqImage: Flamingos2DynamicConfig =
    flamingos2Science(AcqExposureTime).copy(
      disperser = none,
      filter    = Flamingos2Filter.J,
      fpu       = Flamingos2FpuMask.Imaging,
      decker    = Flamingos2Decker.Imaging
    )

  private val MosAcqMask: Flamingos2DynamicConfig =
    MosAcqImage.copy(fpu = CustomMask, decker = Flamingos2Decker.MOS)

  private val ScienceStepConfig: Json =
    json"""{ "stepType": "SCIENCE" }"""

  private val FlatStepConfig: Json =
    json"""
      {
        "stepType": "GCAL",
        "continuum": ${f2_flat_JH1.gcalConfig.lamp.continuum},
        "arcs": []
      }
    """

  private val ArcStepConfig: Json =
    json"""
      {
        "stepType": "GCAL",
        "continuum": null,
        "arcs": ${f2_arc_JH1.gcalConfig.lamp.arcs.map(_.toList)}
      }
    """

  private def scienceAtom(tcs: TelescopeConfig*): Json =
    Json.obj(
      "description"  -> "ABBA Cycle".asJson,
      "observeClass" -> "SCIENCE".asJson,
      "steps"        -> tcs.toList.map(mosStep(flamingos2Science(ExposureTime), ScienceStepConfig, _, "SCIENCE")).asJson
    )

  // Cals are taken at the last science offset with guiding off.
  private def gcalAtom(tc: TelescopeConfig): Json =
    val at = tc.copy(guiding = Disabled)
    Json.obj(
      "description"  -> "Nighttime Calibrations".asJson,
      "observeClass" -> "NIGHT_CAL".asJson,
      "steps"        -> List(
        mosStep(Flamingos2Flat, FlatStepConfig, at, "NIGHT_CAL"),
        mosStep(Flamingos2Arc,  ArcStepConfig,  at, "NIGHT_CAL")
      ).asJson
    )

  private def tc(qArcsec: BigDecimal, g: StepGuideState): TelescopeConfig =
    TelescopeConfig(Offset(Offset.P.Zero, Offset.Q(Angle.fromBigDecimalArcseconds(qArcsec))), g)

  private def setupWith(mode: String): IO[Observation.Id] =
    setupWithProgram(mode).map(_._2)

  private def setupWithProgram(mode: String): IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgramWithNonPartnerPi(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
    yield (p, o)

  private val SparseFieldMode: String =
    """
      flamingos2Mos: {
        disperser: R1200_JH
        filter: JH
        customMask: { slitWidth: CUSTOM_WIDTH_1_PIX }
      }
    """

  // The same observation as a long slit, for the setup time comparison.
  private val EquivalentLongSlitMode: String =
    """
      flamingos2LongSlit: {
        disperser: R1200_JH
        filter: JH
        fpu: LONG_SLIT_1
      }
    """

  // A crowded field is asked for by writing the nod-to-sky configs explicitly.
  private val CrowdedFieldMode: String =
    """
      flamingos2Mos: {
        disperser: R1200_JH
        filter: JH
        customMask: { slitWidth: CUSTOM_WIDTH_1_PIX }
        explicitTelescopeConfigs: {
          toSky: [
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:   0.0 } }, guiding: ENABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 300.0 } }, guiding: DISABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds: 320.0 } }, guiding: DISABLED },
            { offset: { p: { arcseconds: 0.0 }, q: { arcseconds:   0.0 } }, guiding: ENABLED }
          ]
        }
      }
    """

  private def expectScience(oid: Observation.Id, nextAtom: Json, possibleFuture: List[Json]): IO[Unit] =
    expect(
      user     = pi,
      query    = mosScienceQuery(oid),
      expected =
        Json.obj(
          "executionConfig" -> Json.obj(
            "flamingos2" -> Json.obj(
              "science" -> Json.obj(
                "nextAtom"       -> nextAtom,
                "possibleFuture" -> possibleFuture.asJson,
                "hasMore"        -> false.asJson
              )
            )
          )
        ).asRight
    )

  test("sparse field: an ABBA cycle through the custom mask, then nighttime cals"):
    val abba = List(tc(1.2, Enabled), tc(-1.2, Enabled), tc(-1.2, Enabled), tc(1.2, Enabled))
    setupWith(SparseFieldMode).flatMap: oid =>
      expectScience(oid, scienceAtom(abba*), List(gcalAtom(abba.last)))

  test("crowded field: guiding is off on the sky offsets, and the mask is still on every step"):
    val cycle = List(tc(0, Enabled), tc(300, Disabled), tc(320, Disabled), tc(0, Enabled))
    setupWith(CrowdedFieldMode).flatMap: oid =>
      // Only 2 of the 4 steps are guided, so 2 cycles are needed for the ITC's 4 exposures.
      expectScience(oid, scienceAtom(cycle*), List(scienceAtom(cycle*), gcalAtom(cycle.last)))

  test("acquisition: mask out at q=0 and q=10, breakpoint, then the through-mask pair"):
    setupWith(SparseFieldMode).flatMap: oid =>
      expect(
        user     = pi,
        query    = mosAcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "flamingos2": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${flamingos2ExpectedAcq(MosAcqImage, AcqExposureTime, 0,  0)},
                        ${flamingos2ExpectedAcq(MosAcqImage, AcqExposureTime, 0, 10)},
                        ${flamingos2ExpectedAcq(MosAcqMask,  AcqExposureTime, 0,  0, Breakpoint.Enabled)},
                        ${flamingos2ExpectedAcq(MosAcqMask,  AcqExposureTime, 0, 10)}
                      ]
                    },
                    "possibleFuture": ${
                      List
                        .fill(RepeatingAtomCount):
                          json"""
                            {
                              "description": "Fine Adjustments",
                              "observeClass": "ACQUISITION",
                              "steps": [
                                ${flamingos2ExpectedAcq(MosAcqMask, AcqExposureTime, 0, 0, Breakpoint.Enabled)},
                                ${flamingos2ExpectedAcq(MosAcqMask, AcqExposureTime, 0, 10)}
                              ]
                            }
                          """
                        .asJson
                    },
                    "hasMore": false
                  }
                }
              }
            }
          """.asRight
      )

  private def setupTime(pid: Program.Id, oid: Observation.Id): IO[TimeSpan] =
    runObscalcUpdate(pid, oid) *>
      query(
        pi,
        s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  value {
                    setup { full { seconds } }
                  }
                }
              }
            }
          }
        """
      ).map: json =>
        json.hcursor
          .downFields("observation", "execution", "digest", "value", "setup", "full")
          .require[TimeSpan]

  // No MOS setup time has been measured yet, so the digest borrows the long slit's.
  test("the setup time is the long slit's"):
    assertIOBoolean(
      for
        (pm, om) <- setupWithProgram(SparseFieldMode)
        (pl, ol) <- setupWithProgram(EquivalentLongSlitMode)
        mos      <- setupTime(pm, om)
        ls       <- setupTime(pl, ol)
      yield mos === ls
    )
