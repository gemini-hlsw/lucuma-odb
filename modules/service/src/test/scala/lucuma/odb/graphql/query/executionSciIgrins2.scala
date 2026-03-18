// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Igrins2OffsetMode
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

class executionSciIgrins2 extends ExecutionTestSupportForIgrins2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  // Default NodAlongSlit offsets: ±1.25 arcsec in Q
  val qA = -1.25
  val qB =  1.25

  test("[igrins2] mode nod along slit (4 offsets, 1 cycle)"):
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

  test("[igrins2] mode nod along slit, all offsets off slit"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p,
               Some("""[
                 { p: { arcseconds: 0 }, q: { arcseconds: 10 } },
                 { p: { arcseconds: 0 }, q: { arcseconds: 10 } },
                 { p: { arcseconds: 0 }, q: { arcseconds: 10 } },
                 { p: { arcseconds: 0 }, q: { arcseconds: 10 } }
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
            s"Could not generate a sequence for $oid: At least one exposure must be taken on slit."
          ).asLeft
      )

  test("[igrins2] nod to sky - 2 on target, 1 off, need 4 => 2 cycles"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- setOffsets(o, Igrins2OffsetMode.NodToSky,
               """[
                 { p: { arcseconds:  0 }, q: { arcseconds:  0 } },
                 { p: { arcseconds: 10 }, q: { arcseconds: 10 } },
                 { p: { arcseconds:  0 }, q: { arcseconds:  0 } }
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

  // This is legal though we nay need to forbid setting q larger than the slit?
  test("[igrins2] mode nod along slit, 3 offsets on slit per cycle, needs 2 cycles"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p,
               Some("""[
                 { p: { arcseconds: 0 }, q: { arcseconds: -1 } },
                 { p: { arcseconds: 0 }, q: { arcseconds:  1 } },
                 { p: { arcseconds: 0 }, q: { arcseconds: 10 } },
                 { p: { arcseconds: 0 }, q: { arcseconds: -1 } }
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
