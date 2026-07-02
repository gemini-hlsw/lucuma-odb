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
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.odb.sequence.gnirs.spectroscopy.Acquisition.RepeatingAtomCount

class executionAcqGnirsIfu extends ExecutionTestSupportForGnirs:

  // Return T+C exposure/count directly from the input when T+C ETM is used.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(t, c, _) => IntegrationTime(t, c).some
      case _                                          => none

  // Default IFU observation: camera=SHORT_BLUE, prism=MIRROR, fpu=LOW_RESOLUTION,
  // filter=ORDER3, centralWavelength=2200nm.  Default acq coadds = 1.

  /** Switch a GNIRS IFU observation to HR-IFU (which lives on the 0.05"/pix long camera). */
  def setHighResolutionIfu(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsSpectroscopy: {
                  camera: LONG_BLUE
                  ifu: { fpu: HIGH_RESOLUTION }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

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
          fpuOther
          fpuIfu
          readMode
        }
        stepConfig { stepType }
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
    fpuOther:     Option[String],
    fpuIfu:       Option[String],
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
          "fpuOther":  ${fpuOther.asJson},
          "fpuIfu":    ${fpuIfu.asJson},
          "readMode":  $readMode
        },
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(tc)},
        "observeClass": "ACQUISITION",
        "breakpoint":   $breakpoint
      }
    """

  def tc(pArc: Int, qArc: Int, guiding: StepGuideState): TelescopeConfig =
    telescopeConfig(pArc, qArc, guiding)

  def fieldStep(expµs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int) =
    acqStep(expµs, coadds, filter, "ACQUISITION", "ACQUISITION".some, none, readMode,
      tc(pArc, qArc, StepGuideState.Enabled))

  def throughIfuStep(ifu: String)(expµs: Long, coadds: Int, filter: String, readMode: String, pArc: Int, qArc: Int, breakpoint: String = "DISABLED") =
    acqStep(expµs, coadds, filter, s"${ifu}_IFU", none, ifu.some, readMode,
      tc(pArc, qArc, StepGuideState.Enabled), breakpoint)

  // The offset IFU image (Very Bright / Bright only): a fixed 6s single-coadd exposure,
  // always in H (Order4), 10" west of the target, pausing first (breakpoint) for the
  // operator to apply the offsets measured on the field image.
  def ifuImageStep(ifu: String): Json =
    throughIfuStep(ifu)(6_000_000L, 1, "ORDER4", "BRIGHT", -10, 0, "ENABLED")

  // The HR-IFU alignment flat, expanded from the seeded smart gcal fixture (20s, 2
  // coadds); it keeps the science filter (ORDER3) and its read mode follows from the
  // smart gcal exposure.
  def alignmentFlatStep(ifu: String): Json =
    json"""
      {
        "instrumentConfig": {
          "exposure":  { "microseconds": 20000000 },
          "coadds":    2,
          "filter":    "ORDER3",
          "decker":    ${s"${ifu}_IFU".asJson},
          "fpuOther":  null,
          "fpuIfu":    ${ifu.asJson},
          "readMode":  "BRIGHT"
        },
        "stepConfig": { "stepType": "GCAL" },
        "telescopeConfig": ${expectedTelescopeConfig(TelescopeConfig(Offset.Zero, StepGuideState.Disabled))},
        "observeClass": "ACQUISITION",
        "breakpoint":   "DISABLED"
      }
    """

  def fineAdjustments(ifu: String)(expµs: Long, coadds: Int, filter: String, readMode: String, n: Int): Json =
    List.fill(n):
      json"""
        {
          "description": "Fine Adjustments",
          "observeClass": "ACQUISITION",
          "steps": [
            ${throughIfuStep(ifu)(expµs, coadds, filter, readMode, 0, 0)}
          ]
        }
      """
    .asJson

  // 5_000_000 µs = 5 s → integration 5s ⇒ acquisition mode Bright; the through-IFU steps
  // expose 2× the field image (10s). Both fall in the BRIGHT read-mode range.
  val Brightµs:        Long =  5_000_000L
  val BrightThroughµs: Long = 10_000_000L

  test("LR-IFU Bright acquisition — field, offset IFU image, 2 through-IFU steps"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
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
                      ${fieldStep(Brightµs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${ifuImageStep("LOW_RESOLUTION")},
                      ${throughIfuStep("LOW_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("LOW_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  // 300_000 µs = 0.3 s → integration < 0.5s ⇒ acquisition mode VeryBright.  The field
  // image (0.3s) is VERY_BRIGHT but the through-IFU steps (0.6s) reach the BRIGHT read
  // mode, and the offset IFU image stays in H (Order4) despite the selected H2 filter.
  val VeryBrightµs:        Long = 300_000L
  val VeryBrightThroughµs: Long = 600_000L

  test("LR-IFU Very Bright acquisition — offset IFU image always in H (Order4)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 0.3, 1, 2122)
        _ <- setAcquisitionFilter(o, "H2")
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
                      ${fieldStep(VeryBrightµs, 1, "H2", "VERY_BRIGHT", 0, 0)},
                      ${ifuImageStep("LOW_RESOLUTION")},
                      ${throughIfuStep("LOW_RESOLUTION")(VeryBrightThroughµs, 1, "H2", "BRIGHT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(VeryBrightThroughµs, 1, "H2", "BRIGHT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("LOW_RESOLUTION")(VeryBrightThroughµs, 1, "H2", "BRIGHT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  // 30_000_000 µs = 30 s → integration ≥ 10s ⇒ AUTO resolves to Faint. Sky frames replace
  // the offset IFU image, at the default IFU sky offset (-10", 0").
  val Faintµs:        Long = 30_000_000L
  val FaintThroughµs: Long = 60_000_000L

  test("LR-IFU Faint (auto) acquisition — sky frames at the default IFU sky offset (-10, 0)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setAcquisitionTimeAndCount(o, 30.0, 1, 1645)
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
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", -10, 0)},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", -10, 0, "ENABLED")},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  test("LR-IFU explicit Faint acquisition — sky frames at the configured sky offset"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
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
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 10)},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 10, "ENABLED")},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("LOW_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  test("LR-IFU acquisition coadds come from the ITC exposure count in S/N mode"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setAcquisitionSignalToNoise(o, 100, 1645)
        _ <- setAcquisitionFilter(o, "ORDER4")
      yield o

    // In S/N mode the fake imaging result is IntegrationTime(10s, 6): only the field
    // steps take their coadds from the ITC exposure count (6); the through-IFU steps
    // revert to the explicit acquisition coadds (default 1). The 10s × 6 = 60s
    // integration resolves AUTO to Faint (5 steps with skies).
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
                      { "instrumentConfig": { "coadds": 6 } },
                      { "instrumentConfig": { "coadds": 6 } },
                      { "instrumentConfig": { "coadds": 1 } },
                      { "instrumentConfig": { "coadds": 1 } },
                      { "instrumentConfig": { "coadds": 1 } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("HR-IFU Bright acquisition — leading alignment flat"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setHighResolutionIfu(o)
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
                      ${alignmentFlatStep("HIGH_RESOLUTION")},
                      ${fieldStep(Brightµs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${ifuImageStep("HIGH_RESOLUTION")},
                      ${throughIfuStep("HIGH_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", 0, 0)},
                      ${throughIfuStep("HIGH_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("HIGH_RESOLUTION")(BrightThroughµs, 1, "ORDER4", "BRIGHT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )

  test("HR-IFU Faint (auto) acquisition — alignment flat then sky frames"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setHighResolutionIfu(o)
        _ <- setAcquisitionTimeAndCount(o, 30.0, 1, 1645)
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
                      ${alignmentFlatStep("HIGH_RESOLUTION")},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", -10, 0)},
                      ${fieldStep(Faintµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("HIGH_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", -10, 0, "ENABLED")},
                      ${throughIfuStep("HIGH_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)},
                      ${throughIfuStep("HIGH_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", 0, 0)}
                    ]
                  },
                  "possibleFuture": ${fineAdjustments("HIGH_RESOLUTION")(FaintThroughµs, 1, "ORDER4", "FAINT", RepeatingAtomCount)},
                  "hasMore": false
                }
              }
            }
          }
        """.asRight
      )
