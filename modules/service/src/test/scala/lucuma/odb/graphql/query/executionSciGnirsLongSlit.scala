// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
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
      fpuIfu              = None,
      prism               = Some("MIRROR"),
      grating             = Some("D111"),
      mirrorWavelengthNm  = Some(BigDecimal("2200.000")),
      camera              = "SHORT_BLUE",
      focus               = None,
      readMode            = "FAINT"
    )

  // Inline "Nighttime Calibrations" for the default config (2200 nm, SHALLOW,
  // D111, MIRROR, 0.15"/pix, 0.30" slit): one flat (20s, 2 coadds) then one arc
  // (10s, 3 coadds), taken unguided at the last science offset.  The coadds come
  // from the smart gcal fixture, not the science config (1 coadd).  See the
  // smart gcal fixture in ExecutionTestSupportForGnirs.
  private def calAtom(p: BigDecimal, q: BigDecimal): Json =
    gnirsExpectedCalAtom(DynamicSnapshot, p, q, 20.secondTimeSpan, 2, 1, 10.secondTimeSpan, 3, 1)

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

  // thermal-IR (L/M-band) long-camera config
  // 0.05"/pix (LONG_BLUE), D10, MIRROR, 0.20" slit, 3.3 µm.
  // This config has a flat but no arc in the smart gcal tables.
  val ThermalIrSnapshot: GnirsDynamicSnapshot =
    GnirsDynamicSnapshot(
      exposureTime        = ExposureTime,
      coadds              = 1,
      centralWavelengthNm = BigDecimal("3300.000"),
      filter              = "ORDER3",
      decker              = "LONG_CAM_LONG_SLIT",
      fpuSlit             = Some("LONG_SLIT_0_20"),
      fpuOther            = None,
      fpuIfu              = None,
      prism               = Some("MIRROR"),
      grating             = Some("D10"),
      mirrorWavelengthNm  = Some(BigDecimal("3300.000")),
      camera              = "LONG_BLUE",
      focus               = None,
      readMode            = "FAINT"
    )

  test("[gnirs] thermal-IR config yields flat-only calibrations, no arc"):
    // Thermal-IR (L/M-band) configs have a flat but no arc in the smart gcal tables.
    // The sequence must still generate, the arc is optional
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- configureGnirsThermalIr(oid)
      yield oid

    // The flat, with no trailing arc, taken at the last (long-camera) offset.
    val flatOnlyCalAtom: Json =
      gnirsExpectedCalAtom(ThermalIrSnapshot, 0, -1, 20.secondTimeSpan, 2, 1, 10.secondTimeSpan, 3, 0)

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gnirsExpectedScienceAtom(ThermalIrSnapshot,
                    (0, -1, Enabled), (0, 5, Enabled), (0, 5, Enabled), (0, -1, Enabled)
                  ),
                  "possibleFuture" -> List(flatOnlyCalAtom).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] 111/LXD long camera yields arc-only calibrations, no flat"):
    // 0.05"/pix + 111/LXD + 0.675" has arcs but no slit flat (the 111/LXD flat
    // block only covers 0.10" + pinhole), which science confirmed is correct.
    val calLampQuery =
      """
        description
        steps {
          stepConfig {
            ... on Gcal {
              continuum
              arcs
            }
          }
        }
      """

    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- configureGnirsCrossDispersed(oid, "LONG_BLUE")
      yield oid

    setup.flatMap: oid =>
      query(pi, executionConfigQuery(oid, "gnirs", "science", calLampQuery, None)).map: js =>
        val continua = js.findAllByKey("continuum")
        val arcs     = js.findAllByKey("arcs")
        assert(continua.nonEmpty, "expected at least one Gcal step, found none")
        assert(continua.forall(_.isNull), s"expected no flat steps, got continua=$continua")
        assert(arcs.exists(a => a.asArray.exists(_.nonEmpty)), s"expected an arc step, got arcs=$arcs")

  test("[gnirs] 111/LXD short camera has neither flat nor arc -> sequence error"):
    // The 111/LXD rows exist only for the long camera (0.05"/pix), so the same
    // config on the short camera resolves to neither flat nor arc.
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- configureGnirsCrossDispersed(oid, "SHORT_BLUE")
      yield oid

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: missing Smart GCAL mapping: Gnirs { pixelScale: PixelScale_0_15, disperser: D111, crossDispersed: Lxd, wavelength: 1600.000 nm, fpu: LongSlit_0_675, wellDepth: Shallow } (a flat or an arc is required)"
          ).asLeft
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

  test("[gnirs] off-slit offsets don't contribute to S/N (extra cycles)"):
    // The SHORT_BLUE + MIRROR slit is 99" long, so |q| > 49.5" falls off slit.
    // Here q=+2 is on slit but q=+60 is off, so only 1 of the 2 steps per cycle
    // is on source. exposureCount=3 therefore needs 3 cycles (not 2).
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setAlongSlitTelescopeConfigs(oid,
                 """[
                   { q: { arcseconds:  2 }, guiding: ENABLED  },
                   { q: { arcseconds: 60 }, guiding: DISABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, 2, Enabled), (0, 60, Disabled)
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
                  "possibleFuture" -> List(expectedAtom, expectedAtom, calAtom(0, 60)).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] all along-slit offsets off slit -> error"):
    // When no science step lands on the slit there are no on-source exposures,
    // so the sequence cannot be generated.
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setAlongSlitTelescopeConfigs(oid,
                 """[
                   { q: { arcseconds: 60 }, guiding: DISABLED },
                   { q: { arcseconds: 60 }, guiding: DISABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: At least one exposure must be on slit (if longslit) or guided (if IFU)."
          ).asLeft
      )

  test("[gnirs] telluric sequences omit the inline flats & arcs"):
    // A GNIRS telluric is a standard-star observation; its flats & arcs come
    // with the associated science, so its sequence must contain only science
    // cycles (no "Nighttime Calibrations" atoms).  The science steps are
    // charged as NIGHT_CAL because of the telluric calibration role.
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setObservationCalibrationRole(List(oid), CalibrationRole.Telluric)
      yield oid

    // Compact projection: enough to assert the *shape* of the sequence and, in
    // particular, that no calibration atom is present.
    val atomShapeQuery: String =
      """
        description
        observeClass
        steps {
          stepConfig { stepType }
          observeClass
        }
      """

    setup.flatMap: oid =>
      val sciStep: Json =
        Json.obj(
          "stepConfig"   -> Json.obj("stepType" -> "SCIENCE".asJson),
          "observeClass" -> "NIGHT_CAL".asJson
        )
      val scienceAtom: Json =
        Json.obj(
          "description"  -> "Science Cycle".asJson,
          "observeClass" -> "NIGHT_CAL".asJson,
          "steps"        -> List.fill(4)(sciStep).asJson
        )
      expect(
        user     = pi,
        query    = executionConfigQuery(oid, "gnirs", "science", atomShapeQuery, None),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> scienceAtom,
                  "possibleFuture" -> Json.arr(),  // no "Nighttime Calibrations" atom
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] nod-to-sky offsets carry full P/Q + per-entry guiding"):
    // The sky position (p=60) is off target, so only the on-axis (p=0) step
    // contributes to the S/N: exposureCount=3 requires 3 cycles.
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
                  "possibleFuture" -> List(expectedAtom, expectedAtom, calAtom(60, 60)).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] nod-to-sky sky position off the slit end (p=0, q>slit) counts as sky"):
    // A sky nod along the slit direction (p=0) that runs past the slit end is
    // off slit and so doesn't contribute to the S/N — the q check matters, not
    // just p. The SHORT_BLUE + MIRROR slit is 99", so slit/2 = 49.5"; q = 49.6"
    // is just one deci-arcsecond past the edge and therefore off slit. Only the
    // on-axis step is on source, so exposureCount=3 needs 3 cycles.
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- setToSkyTelescopeConfigs(oid,
                 """[
                   { offset: { p: { arcseconds: 0 }, q: { arcseconds:  0   } }, guiding: ENABLED  },
                   { offset: { p: { arcseconds: 0 }, q: { arcseconds: 49.6 } }, guiding: DISABLED }
                 ]"""
               )
      yield oid

    setup.flatMap: oid =>
      val expectedAtom = gnirsExpectedScienceAtom(DynamicSnapshot,
        (0, 0, Enabled), (0, 49.6, Disabled)
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
                  "possibleFuture" -> List(expectedAtom, expectedAtom, calAtom(0, BigDecimal("49.6"))).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[gnirs] two central wavelengths -> round-robined segments, each with its own calibrations"):
    // Each central wavelength is its own configuration, so it runs as a
    // contiguous segment closed by its own flat + arc, and the segments are
    // round-robined.  With one cycle needed at each, the whole sequence is
    // sci(2200), cal(2200), sci(2300), cal(2300).
    val setup: IO[Observation.Id] =
      for
        oid <- gnirsObs
        _   <- query(
                 pi,
                 s"""
                   mutation {
                     updateObservations(input: {
                       SET: {
                         observingMode: {
                           gnirsSpectroscopy: {
                             centralWavelengths: [
                               {
                                 centralWavelength: { nanometers: 2200 }
                                 exposureTimeMode: { timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2200 } } }
                               }
                               {
                                 centralWavelength: { nanometers: 2300 }
                                 exposureTimeMode: { timeAndCount: { time: { seconds: 30.0 } count: 3 at: { nanometers: 2300 } } }
                               }
                             ]
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
      yield oid

    // With more than one wavelength the atom titles carry it, so the observer
    // can tell the segments apart.
    def titled(atom: Json, description: String): Json =
      atom.deepMerge(Json.obj("description" -> description.asJson))

    val snapshot2300: GnirsDynamicSnapshot =
      DynamicSnapshot.copy(
        centralWavelengthNm = BigDecimal("2300.000"),
        mirrorWavelengthNm  = Some(BigDecimal("2300.000"))
      )

    setup.flatMap: oid =>
      val sci2200 = titled(
        gnirsExpectedScienceAtom(DynamicSnapshot, (0, 2, Enabled), (0, -4, Enabled), (0, -4, Enabled), (0, 2, Enabled)),
        "Science Cycle (2200 nm)"
      )
      val cal2200 = titled(calAtom(0, 2), "Nighttime Calibrations (2200 nm)")
      val sci2300 = titled(
        gnirsExpectedScienceAtom(snapshot2300, (0, 2, Enabled), (0, -4, Enabled), (0, -4, Enabled), (0, 2, Enabled)),
        "Science Cycle (2300 nm)"
      )
      val cal2300 = titled(
        gnirsExpectedCalAtom(snapshot2300, 0, 2, 20.secondTimeSpan, 2, 1, 10.secondTimeSpan, 3, 1),
        "Nighttime Calibrations (2300 nm)"
      )

      expect(
        user     = pi,
        query    = gnirsScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gnirs" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> sci2200,
                  "possibleFuture" -> List(cal2200, sci2300, cal2300).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )
