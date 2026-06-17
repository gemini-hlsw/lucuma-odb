// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.odb.sequence.gnirs.longslit.Acquisition.RepeatingAtomCount

class executionAcqGnirs extends ExecutionTestSupportForGnirs:

  // Return T+C exposure/count directly from the input when T+C ETM is used.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(t, c, _) => IntegrationTime(t, c).some
      case _                                          => none

  // Default observation: camera=SHORT_BLUE, prism=MIRROR, fpu=LONG_SLIT_0_30.
  // slitDecker = SHORT_CAM_LONG_SLIT; acquisitionMirror=In.
  // Default acq coadds = 1.

  // Narrow query used in most tests — omits fields with tricky formatting (centralWavelength, etc).
  val AcqStepsQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { microseconds }
          coadds
          filter
          decker
          fpuSlit
          fpuOther
          readMode
        }
        telescopeConfig {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
          guiding
        }
        observeClass
        breakpoint
      }
    """

  def gnirsAcqNarrowQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "gnirs", "acquisition", AcqStepsQuery, futureLimit)

  def acqStep(
    microseconds: Long,
    coadds:       Int,
    filter:       String,
    decker:       String,
    fpuSlit:      Option[String],
    fpuOther:     Option[String],
    readMode:     String,
    tc:           TelescopeConfig,
    breakpoint:   String = "DISABLED"
  ): Json =
    json"""
      {
        "instrumentConfig": {
          "exposure":  { "microseconds": $microseconds },
          "coadds":    $coadds,
          "filter":    $filter,
          "decker":    $decker,
          "fpuSlit":   ${fpuSlit.asJson},
          "fpuOther":  ${fpuOther.asJson},
          "readMode":  $readMode
        },
        "telescopeConfig": ${expectedTelescopeConfig(tc)},
        "observeClass": "ACQUISITION",
        "breakpoint":   $breakpoint
      }
    """

  def tc(pArc: Int, qArc: Int, guiding: StepGuideState): TelescopeConfig =
    telescopeConfig(pArc, qArc, guiding)

  def slitImgStep(expµs: Long, coadds: Int, filter: String, readMode: String) =
    acqStep(expµs, coadds, filter, "SHORT_CAM_LONG_SLIT", "LONG_SLIT_0_30".some, none, readMode,
      tc(10, 0, StepGuideState.Disabled))

  def fieldStep(expµs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int) =
    acqStep(expµs, coadds, filter, "ACQUISITION", none, "ACQUISITION".some, readMode,
      tc(pArc, qArc, StepGuideState.Enabled))

  def throughSlitStep(expµs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int, breakpoint: String = "DISABLED") =
    acqStep(expµs, coadds, filter, "SHORT_CAM_LONG_SLIT", "LONG_SLIT_0_30".some, none, readMode,
      tc(pArc, qArc, StepGuideState.Enabled), breakpoint)

  // Fixed FPU-image (first acquisition step) exposure times for the short camera
  // (SHORT_BLUE): X=10s, J=15s, H=3s, K=3s. All fall in the BRIGHT read-mode range.
  val XShortµs: Long = 10_000_000L
  val JShortµs: Long = 15_000_000L
  val HShortµs: Long =  3_000_000L
  val KShortµs: Long =  3_000_000L

  // Fixed FPU-image exposure times for the long camera (LONG_BLUE): H=15s, K=15s (both
  // BRIGHT), PAH=0.5s (VERY_BRIGHT). X, J and H2 all fall back to H (15s) on the long camera.
  val HLongµs:   Long = 15_000_000L
  val KLongµs:   Long = 15_000_000L
  val PahLongµs: Long =    500_000L

  // 5_000_000 µs = 5 s  → Bright (1s < 5s ≤ 10s), readMode=BRIGHT
  val Brightµs: Long = 5_000_000L

  test("Bright acquisition — initial atom (3 steps, no sky)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 5.0, 1, 1645)
        _ <- setAcquisitionFilter(o, "ORDER4")
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsAcqNarrowQuery(oid),
        expected = json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": {
                  "nextAtom": {
                    "description": "Initial Acquisition",
                    "observeClass": "ACQUISITION",
                    "steps": [
                      ${slitImgStep(HShortµs, 1, "ORDER4", "BRIGHT")},
                      ${fieldStep(Brightµs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${throughSlitStep(Brightµs, 1, "ORDER4", "BRIGHT", 0, 0, "ENABLED")}
                    ]
                  },
                  "possibleFuture": ${brightFineAdjustments(RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  def brightFineAdjustments(n: Int): Json =
    List.fill(n):
      json"""
        {
          "description": "Fine Adjustments",
          "observeClass": "ACQUISITION",
          "steps": [
            ${throughSlitStep(Brightµs, 1, "ORDER4", "BRIGHT", 0, 0)}
          ]
        }
      """
    .asJson

  // 30_000_000 µs = 30 s → Faint (> 10s), readMode=FAINT
  val Faintµs: Long = 30_000_000L

  test("Faint acquisition with sky offset — initial atom (5 steps with sky subtraction)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 30.0, 1, 1645)
        _ <- setAcquisitionFilter(o, "ORDER4")
        _ <- setAcquisitionFaint(o, 0, 10)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsAcqNarrowQuery(oid),
        expected = json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": {
                  "nextAtom": {
                    "description": "Initial Acquisition",
                    "observeClass": "ACQUISITION",
                    "steps": [
                      ${slitImgStep(HShortµs, 1, "ORDER4", "BRIGHT")},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 10)},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughSlitStep(Faintµs, 1, "ORDER4", "FAINT", 0, 10)},
                      ${throughSlitStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0, "ENABLED")}
                    ]
                  },
                  "possibleFuture": ${faintFineAdjustments(RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  def faintFineAdjustments(n: Int): Json =
    List.fill(n):
      json"""
        {
          "description": "Fine Adjustments",
          "observeClass": "ACQUISITION",
          "steps": [
            ${throughSlitStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0)}
          ]
        }
      """
    .asJson

  // 500_000 µs = 0.5 s. Exposure < 0.6s ⇒ readMode=VERY_BRIGHT. (Note: integration time
  // 0.5s is NOT < 0.5, so the acquisition *mode* resolves to Bright, not VeryBright.)
  val VeryBrightµs: Long = 500_000L

  // 300_000 µs = 0.3 s. Integration time < 0.5s ⇒ acquisition mode VeryBright; the exposure
  // is also < 0.6s ⇒ readMode=VERY_BRIGHT.
  val VeryBrightAcqµs: Long = 300_000L

  // A short (0.5s) ITC exposure yields VERY_BRIGHT science read mode (exposure < 0.6s),
  // while the acquisition mode is Bright (integration time 0.5s is not < 0.5s) and the H2
  // filter falls back to H (Order4) for the FPU image.
  test("Short ITC exposure yields VERY_BRIGHT science read mode"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 0.5, 1, 2122)
        _ <- setAcquisitionFilter(o, "H2")
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gnirs {
                acquisition {
                  nextAtom {
                    steps {
                      instrumentConfig { exposure { microseconds } readMode filter }
                      telescopeConfig  { offset { p { arcseconds } q { arcseconds } } guiding }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      {
                        "instrumentConfig": { "exposure": { "microseconds": $HShortµs }, "readMode": "BRIGHT", "filter": "ORDER4" },
                        "telescopeConfig": { "offset": { "p": { "arcseconds": 10.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "DISABLED" }
                      },
                      {
                        "instrumentConfig": { "exposure": { "microseconds": $VeryBrightµs }, "readMode": "VERY_BRIGHT", "filter": "H2" },
                        "telescopeConfig": { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "ENABLED" }
                      },
                      {
                        "instrumentConfig": { "exposure": { "microseconds": $VeryBrightµs }, "readMode": "VERY_BRIGHT", "filter": "H2" },
                        "telescopeConfig": { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "ENABLED" }
                      }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("Acquisition coadds come from acquisition config, not ITC"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 5.0, 1, 1645)
        _ <- setAcquisitionFilter(o, "ORDER4")
        _ <- query(
               pi,
               s"""
                 mutation {
                   updateObservations(input: {
                     SET: {
                       observingMode: {
                         gnirsLongSlit: {
                           acquisition: { coadds: 7 }
                         }
                       }
                     }
                     WHERE: { id: { EQ: "$o" } }
                   }) { observations { id } }
                 }
               """
             ).void
      yield o

    // 5s exposure × 7 coadds = 35s integration ≥ 10s ⇒ AUTO resolves to FAINT
    // (a sky frame is added, so 5 steps); every step except the FPU image takes its
    // coadds from the acquisition config (7), not from the ITC. The FPU image (first
    // step) always uses a single coadd.
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gnirs {
                acquisition {
                  nextAtom {
                    steps {
                      instrumentConfig { coadds }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      { "instrumentConfig": { "coadds": 1 } },
                      { "instrumentConfig": { "coadds": 7 } },
                      { "instrumentConfig": { "coadds": 7 } },
                      { "instrumentConfig": { "coadds": 7 } },
                      { "instrumentConfig": { "coadds": 7 } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  // Focused query checking the first-atom step exposure/coadds/filter/readMode.
  def firstAtomConfigQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: "$oid") {
          gnirs { acquisition { nextAtom { steps {
            instrumentConfig { exposure { microseconds } coadds filter readMode }
          } } } }
        }
      }
    """

  def firstAtomConfig(expµs: Long, coadds: Int, filter: String, readMode: String): Json =
    json"""{ "instrumentConfig": { "exposure": { "microseconds": $expµs }, "coadds": $coadds, "filter": $filter, "readMode": $readMode } }"""

  // The FPU image (first step) filter and exposure as a function of (camera, selected
  // acquisition filter), in Bright mode (5s ITC). The first step always uses 1 coadd and
  // a read mode derived from its fixed exposure; the remaining steps keep the selected
  // filter and the ITC exposure/coadds (BRIGHT). Columns:
  //   camera, selected filter, FPU-image filter, FPU-image exposure µs, FPU-image readMode
  List(
    // Short camera: X=10s, J=15s, H=3s, K=3s, H2→H(3s). All BRIGHT.  The X/J/H/K bands are
    // the spectroscopy order filters Order6/Order5/Order4/Order3 (the acquisition filters).
    ("SHORT_BLUE", "ORDER6", "ORDER6", XShortµs,  "BRIGHT"),      // X stays X
    ("SHORT_BLUE", "ORDER5", "ORDER5", JShortµs,  "BRIGHT"),      // J (Order5) stays Order5
    ("SHORT_BLUE", "ORDER4", "ORDER4", HShortµs,  "BRIGHT"),      // H
    ("SHORT_BLUE", "ORDER3", "ORDER3", KShortµs,  "BRIGHT"),      // K (Order3) stays Order3
    ("SHORT_BLUE", "H2",     "ORDER4", HShortµs,  "BRIGHT"),      // H2 → H
    // Long camera: X→H, J→H, H=15s, K=15s, H2→H(15s), PAH=0.5s.
    ("LONG_BLUE",  "ORDER6", "ORDER4", HLongµs,   "BRIGHT"),      // X → H
    ("LONG_BLUE",  "ORDER5", "ORDER4", HLongµs,   "BRIGHT"),      // J (Order5) → H
    ("LONG_BLUE",  "ORDER4", "ORDER4", HLongµs,   "BRIGHT"),      // H
    ("LONG_BLUE",  "ORDER3", "ORDER3", KLongµs,   "BRIGHT"),      // K (Order3) stays Order3
    ("LONG_BLUE",  "H2",     "ORDER4", HLongµs,   "BRIGHT"),      // H2 → H
    ("LONG_BLUE",  "PAH",    "PAH",    PahLongµs, "VERY_BRIGHT")  // PAH, 0.5s ⇒ VERY_BRIGHT
  ).foreach: (camera, selected, fpuFilter, fpuµs, fpuReadMode) =>
    test(s"$camera Bright acquisition with $selected filter — FPU image filter/exposure"):
      val setup: IO[Observation.Id] =
        for
          p <- createProgram
          t <- createTargetWithProfileAs(pi, p)
          o <- createGnirsLongSlitObservationAs(pi, p, t)
          _ <- setCamera(o, camera)
          _ <- setAcquisitionTimeAndCount(o, 5.0, 1, 1645)
          _ <- setAcquisitionFilter(o, selected)
        yield o

      setup.flatMap: oid =>
        expect(
          user     = pi,
          query    = firstAtomConfigQuery(oid),
          expected = json"""
            {
              "executionConfig": { "gnirs": { "acquisition": { "nextAtom": { "steps": [
                ${firstAtomConfig(fpuµs,    1, fpuFilter, fpuReadMode)},
                ${firstAtomConfig(Brightµs, 1, selected,  "BRIGHT")},
                ${firstAtomConfig(Brightµs, 1, selected,  "BRIGHT")}
              ] } } } }
            }
          """.asRight
        )

  test("VeryBright acquisition forces the FPU image to H even with an explicit non-H filter"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, BigDecimal(VeryBrightAcqµs) / 1_000_000, 1, 1645) // 0.3s ⇒ VeryBright
        _ <- setAcquisitionFilter(o, "ORDER6")           // X would otherwise be 10s on the short camera
      yield o

    // VeryBright always images the FPU in H (Order4) at the short-camera H exposure (3s);
    // the remaining steps still use the selected X filter at the ITC (VeryBright) exposure.
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = firstAtomConfigQuery(oid),
        expected = json"""
          {
            "executionConfig": { "gnirs": { "acquisition": { "nextAtom": { "steps": [
              ${firstAtomConfig(HShortµs,        1, "ORDER4", "BRIGHT")},
              ${firstAtomConfig(VeryBrightAcqµs, 1, "ORDER6", "VERY_BRIGHT")},
              ${firstAtomConfig(VeryBrightAcqµs, 1, "ORDER6", "VERY_BRIGHT")}
            ] } } } }
          }
        """.asRight
      )

  // PAH on the short camera is always an error, regardless of mode (including VeryBright,
  // which otherwise images the FPU in H). Columns: ITC seconds, acquisition-type label.
  List(
    (5.0, "Bright"),     // 5s   ⇒ Bright
    (30.0, "Faint"),     // 30s  ⇒ Faint
    (0.5, "Very Bright") // 0.5s ⇒ VeryBright
  ).foreach: (seconds, acqType) =>
    test(s"PAH acquisition filter cannot be used with the short camera ($acqType)"):
      val setup: IO[Observation.Id] =
        for
          p <- createProgram
          t <- createTargetWithProfileAs(pi, p)
          o <- createGnirsLongSlitObservationAs(pi, p, t)
          _ <- setAcquisitionTimeAndCount(o, seconds, 1, 1645)
          _ <- setAcquisitionFilter(o, "PAH")
        yield o

      setup.flatMap: oid =>
        expect(
          user     = pi,
          query    = firstAtomConfigQuery(oid),
          expected = List(
            s"Could not generate a sequence for $oid: PAH acquisition filter cannot be used with short camera"
          ).asLeft
        )
