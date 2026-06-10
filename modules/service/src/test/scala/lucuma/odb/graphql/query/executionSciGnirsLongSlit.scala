// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * Tests GNIRS LongSlit science sequence generation.  The default GNIRS
 * observation is created with timeAndCount(time = 30s, count = 3) and uses
 * the default offsets for camera=ShortBlue, prism=Mirror, filter=Order3,
 * which is the "short camera long slit" pattern: [+2, -4, -4, +2] in Q.
 */
class executionSciGnirsLongSlit extends ExecutionTestSupportForGnirs:

  val ExposureTime: TimeSpan = 30.secondTimeSpan

  // Default GNIRS observation:
  //   grating=D111, prism=MIRROR, camera=SHORT_BLUE, fpu=LONG_SLIT_0_30,
  //   filter=ORDER3, timeAndCount(time=30s, count=3, at=2200nm)
  // Defaults computed from this for our science step:
  //   decker=SHORT_CAM_LONG_SLIT, readMode resolved per exposure time,
  //   coadds=1, well depth = SHALLOW.
  // Offset list for camera=ShortBlue (non-XD), filter=ORDER3 is the
  // "short camera long slit" pattern: q=[+2, -4, -4, +2].
  val DynamicSnapshot: GnirsDynamicSnapshot =
    GnirsDynamicSnapshot(
      exposureTime        = ExposureTime,
      coadds              = 1,
      centralWavelengthNm = BigDecimal("2200.000"),
      filter              = "ORDER3",
      decker              = "SHORT_CAM_LONG_SLIT",
      fpuSlit             = Some("LONG_SLIT_0_30"),
      fpuOther            = None,
      prism               = Some("MIRROR"),
      grating             = Some("D111"),
      mirrorWavelengthNm  = Some(BigDecimal("2200.000")),
      camera              = "SHORT_BLUE",
      focus               = None,
      readMode            = "FAINT"
    )

  // Inline "Nighttime Calibrations" for the default config (2200 nm, SHALLOW,
  // D111, MIRROR, 0.15"/pix, 0.30" slit): one flat (20s) then one arc (10s),
  // taken unguided at the last science offset.  See the smart gcal fixture in
  // ExecutionTestSupportForGnirs.
  private def calAtom(p: BigDecimal, q: BigDecimal): Json =
    gnirsExpectedCalAtom(DynamicSnapshot, p, q, 20.secondTimeSpan, 1, 10.secondTimeSpan, 1)

  // Default offset pattern's last position is q = +2.
  val DefaultCalAtom: Json = calAtom(0, 2)

  private def gnirsObs: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGnirsLongSlitObservationAs(pi, p, t)
    yield o

  test("[gnirs] short camera default offsets, exposureCount=3 -> 1 cycle of 4"):
    gnirsObs.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gnirsExpectedScienceAtom(DynamicSnapshot,
                    (0, 2, Enabled), (0, -4, Enabled), (0, -4, Enabled), (0, 2, Enabled)
                  ),
                  "possibleFuture" -> List(DefaultCalAtom).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] short camera default offsets, exposureCount=3 -> 1 cycle of 4, unsplittable"):
    for
      o <- gnirsObs
      _ <- setIsSplittableAs(pi, o, isSplittable = false)
      _ <- expect(
        user     = pi,
        query    = gnirsScienceQuery(o),
        expected = expectedUnsplittableExecutionConfig(
          "gnirs",
          gnirsExpectedScienceAtom(
            DynamicSnapshot,
            (0,  2, Enabled),
            (0, -4, Enabled),
            (0, -4, Enabled),
            (0,  2, Enabled)
          ),
          DefaultCalAtom
        ).asRight
      )
    yield ()

  test("[gnirs] materialized sequence round-trips through t_gnirs_dynamic"):
    // Recording a visit materializes the science sequence into the DB.  The
    // subsequent query must then read it back (via SelectGnirsSequence) and
    // produce exactly the same dynamic config as the freshly generated one.
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- recordVisitAs(serviceUser, oid)
      yield oid

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gnirsExpectedScienceAtom(DynamicSnapshot,
                    (0, 2, Enabled), (0, -4, Enabled), (0, -4, Enabled), (0, 2, Enabled)
                  ),
                  "possibleFuture" -> List(DefaultCalAtom).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] exposureCount=8 with 4 offsets -> 2 identical cycles"):
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setScienceTimeAndCount(oid, BigDecimal(30), 8, BigDecimal(2200))
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, 2, Enabled), (0, -4, Enabled), (0, -4, Enabled), (0, 2, Enabled)
      )
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> expectedAtom,
                  "possibleFuture" -> List(expectedAtom, DefaultCalAtom).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] explicit along-slit offsets (2 entries) — exposureCount=3 rounds up to 2 cycles"):
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setAlongSlitTelescopeConfigs(oid,
                 """[
                   { q: { arcseconds: -2 }, guiding: ENABLED },
                   { q: { arcseconds:  2 }, guiding: ENABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, -2, Enabled), (0, 2, Enabled)
      )
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> expectedAtom,
                  "possibleFuture" -> List(expectedAtom, calAtom(0, 2)).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] all offsets off-slit are cycled blindly (no error)"):
    // Deliberate divergence from IGRINS-2: GNIRS does not filter off-slit
    // offsets out of the cycle.  Provide |q| > slit length and observe that
    // we still get a valid atom (rather than a "must be on slit" error).
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setAlongSlitTelescopeConfigs(oid,
                 """[
                   { q: { arcseconds: 10 }, guiding: ENABLED },
                   { q: { arcseconds: 20 }, guiding: ENABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, 10, Enabled), (0, 20, Enabled)
      )
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> expectedAtom,
                  "possibleFuture" -> List(expectedAtom, calAtom(0, 20)).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] nod-to-sky offsets carry full P/Q + per-entry guiding"):
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setToSkyTelescopeConfigs(oid,
                 """[
                   { offset: { p: { arcseconds: 0 },  q: { arcseconds: 0 }  }, guiding: ENABLED  },
                   { offset: { p: { arcseconds: 60 }, q: { arcseconds: 60 } }, guiding: DISABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, 0, Enabled), (60, 60, Disabled)
      )
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> expectedAtom,
                  "possibleFuture" -> List(expectedAtom, calAtom(60, 60)).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )
