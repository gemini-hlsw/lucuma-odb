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

  def slitImgStep(expUs: Long, coadds: Int, filter: String, readMode: String) =
    acqStep(expUs, coadds, filter, "SHORT_CAM_LONG_SLIT", "LONG_SLIT_0_30".some, none, readMode,
      tc(10, 0, StepGuideState.Disabled))

  def fieldStep(expUs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int) =
    acqStep(expUs, coadds, filter, "ACQUISITION", none, "ACQUISITION".some, readMode,
      tc(pArc, qArc, StepGuideState.Enabled))

  def throughSlitStep(expUs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int, breakpoint: String = "DISABLED") =
    acqStep(expUs, coadds, filter, "SHORT_CAM_LONG_SLIT", "LONG_SLIT_0_30".some, none, readMode,
      tc(pArc, qArc, StepGuideState.Enabled), breakpoint)

  // 5_000_000 µs = 5 s  → Bright (1s < 5s ≤ 10s), readMode=BRIGHT
  val BrightUs: Long = 5_000_000L

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
                      ${slitImgStep(BrightUs, 1, "ORDER4", "BRIGHT")},
                      ${fieldStep(BrightUs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${throughSlitStep(BrightUs, 1, "ORDER4", "BRIGHT", 0, 0, "ENABLED")}
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
            ${throughSlitStep(BrightUs, 1, "ORDER4", "BRIGHT", 0, 0)}
          ]
        }
      """
    .asJson

  // 30_000_000 µs = 30 s → Faint (> 10s), readMode=FAINT
  val FaintUs: Long = 30_000_000L

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
                      ${slitImgStep(FaintUs, 1, "ORDER4", "FAINT")},
                      ${fieldStep(FaintUs, 1, "ORDER4", "FAINT", 0, 10)},
                      ${fieldStep(FaintUs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughSlitStep(FaintUs, 1, "ORDER4", "FAINT", 0, 10)},
                      ${throughSlitStep(FaintUs, 1, "ORDER4", "FAINT", 0, 0, "ENABLED")}
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
            ${throughSlitStep(FaintUs, 1, "ORDER4", "FAINT", 0, 0)}
          ]
        }
      """
    .asJson

  // 500_000 µs = 0.5 s → VeryBright (< 1s), readMode=VERY_BRIGHT
  val VeryBrightUs: Long = 500_000L

  test("VeryBright acquisition (< 1s) — VERY_BRIGHT readMode"):
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
                        "instrumentConfig": { "exposure": { "microseconds": $VeryBrightUs }, "readMode": "VERY_BRIGHT", "filter": "ORDER4" },
                        "telescopeConfig": { "offset": { "p": { "arcseconds": 10.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "DISABLED" }
                      },
                      {
                        "instrumentConfig": { "exposure": { "microseconds": $VeryBrightUs }, "readMode": "VERY_BRIGHT", "filter": "H2" },
                        "telescopeConfig": { "offset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 0.000000 } }, "guiding": "ENABLED" }
                      },
                      {
                        "instrumentConfig": { "exposure": { "microseconds": $VeryBrightUs }, "readMode": "VERY_BRIGHT", "filter": "H2" },
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
    // (a sky frame is added, so 5 steps); every step takes its coadds from the
    // acquisition config (7), not from the ITC.
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
                      { "instrumentConfig": { "coadds": 7 } },
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
