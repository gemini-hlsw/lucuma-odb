// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.refined.*

class executionSciIgrins2 extends ExecutionTestSupportForIgrins2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, 4.refined)

  // q components of the ABBA pattern
  val qA = -1.25
  val qB =  1.25

  test("[igrins2] mode nod along slit (4 offsets, 1 atom)"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
      } yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, qA, Enabled), (0, qB, Enabled), (0, qB, Enabled), (0, qA, Enabled)
                  ),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] mode nod along slit (4 offsets, 1 atom), unsplittable"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- setIsSplittableAs(pi, o, isSplittable = false)
      } yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected = expectedUnsplittableExecutionConfig(
          "igrins2",
          igrins2ExpectedScienceAtom(
            ExposureTime,
            (0, qA, Enabled),
            (0, qB, Enabled),
            (0, qB, Enabled),
            (0, qA, Enabled)
          )
        ).asRight
      )

  test("[igrins2] mode nod along slit, all offsets off slit"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p,
               Some("""[
                 { q: { arcseconds: 10 }, guiding: DISABLED },
                 { q: { arcseconds: 10 }, guiding: DISABLED },
                 { q: { arcseconds: 10 }, guiding: DISABLED },
                 { q: { arcseconds: 10 }, guiding: DISABLED }
               ]"""),
               t
             )
      } yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: At least one exposure must be on slit (if longslit) or guided (if IFU)."
          ).asLeft
      )

  test("[igrins2] nod to sky - 2 on target, 1 off"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- setOffsets(o, SlitOffsetMode.NodToSky,
               """[
                 { offset: { p: { arcseconds:  0 }, q: { arcseconds:  0 } }, guiding: ENABLED  },
                 { offset: { p: { arcseconds: 10 }, q: { arcseconds: 10 } }, guiding: DISABLED },
                 { offset: { p: { arcseconds:  0 }, q: { arcseconds:  0 } }, guiding: ENABLED  }
               ]"""
             )
      } yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, 0, Enabled), (10, 10, Disabled), (0, 0, Enabled)
                  ),
                  "possibleFuture" -> List(
                    igrins2ExpectedScienceAtom(ExposureTime,
                      (0, 0, Enabled), (10, 10, Disabled), (0, 0, Enabled)
                    )
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] nod to sky, all offsets off target"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- setOffsets(o, SlitOffsetMode.NodToSky,
               """[
                 { offset: { p: { arcseconds: 10 }, q: { arcseconds: 0 } }, guiding: DISABLED },
                 { offset: { p: { arcseconds: 20 }, q: { arcseconds: 0 } }, guiding: DISABLED },
                 { offset: { p: { arcseconds: 10 }, q: { arcseconds: 0 } }, guiding: DISABLED }
               ]"""
             )
      } yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: At least one exposure must be on slit (if longslit) or guided (if IFU)."
          ).asLeft
      )

  test("[igrins2] nod to sky, sky position off the slit end (p=0, q>slit) counts as sky"):
    // A sky nod along the slit direction (p=0) that runs past the slit end is
    // off slit, so it doesn't contribute to the S/N (the q check matters, not
    // just p). The slit is 5", so slit/2 = 2.5"; q = 2.6" is just one
    // deci-arcsecond past the edge and therefore off slit. Only the on-axis
    // step is on source, so exposureCount=4 needs 4 cycles. Under the previous
    // p-only logic q=2.6" (p=0) would have counted as on target -> 2 cycles.
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- setOffsets(o, SlitOffsetMode.NodToSky,
               """[
                 { offset: { p: { arcseconds: 0 }, q: { arcseconds: 0   } }, guiding: ENABLED  },
                 { offset: { p: { arcseconds: 0 }, q: { arcseconds: 2.6 } }, guiding: DISABLED }
               ]"""
             )
      } yield o

    setup.flatMap: oid =>
      val expectedAtom = igrins2ExpectedScienceAtom(ExposureTime,
        (0, 0, Enabled), (0, 2.6, Disabled)
      )
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> expectedAtom,
                  "possibleFuture" -> List(expectedAtom, expectedAtom, expectedAtom).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )

  // This is legal though we nay need to forbid setting q larger than the slit?
  test("[igrins2] mode nod along slit, 3 offsets on slit"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p,
               Some("""[
                 { q: { arcseconds: -1 }, guiding: ENABLED  },
                 { q: { arcseconds:  1 }, guiding: ENABLED  },
                 { q: { arcseconds: 10 }, guiding: DISABLED },
                 { q: { arcseconds: -1 }, guiding: ENABLED  }
               ]"""),
               t
             )
      } yield o

    // 3 on slit, 1 off, to get 4 we need 4 cycles
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtom(ExposureTime,
                    (0, -1, Enabled), (0, 1, Enabled), (0, 10, Disabled), (0, -1, Enabled)
                  ),
                  "possibleFuture" -> List(
                    igrins2ExpectedScienceAtom(ExposureTime,
                      (0, -1, Enabled), (0, 1, Enabled), (0, 10, Disabled), (0, -1, Enabled)
                    )
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("[igrins2] telluric calibration have Night Cal observe class"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        // Directly set the calibration role
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.Telluric)
      } yield o

    // Verify the steps are created with class night cal
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = igrins2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "igrins2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> igrins2ExpectedScienceAtomAs(ObserveClass.NightCal, ExposureTime,
                    (0, qA, Enabled), (0, qB, Enabled), (0, qB, Enabled), (0, qA, Enabled)
                  ),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )
