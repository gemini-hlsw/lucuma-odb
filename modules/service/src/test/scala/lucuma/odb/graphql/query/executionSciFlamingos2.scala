// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

class executionSciFlamingos2 extends ExecutionTestSupportForFlamingos2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("simple generation - limited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, 15, Enabled), (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                  "possibleFuture" -> List(flamingos2ExpectedGcals((0, 15))).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one science"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                  "possibleFuture" -> List(flamingos2ExpectedGcals((0, 15))).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one cycle"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)
        _  <- ss.traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedGcals((0, 15)),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one sequence"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, o)
        ss <- scienceStepIds(serviceUser, o)
        _  <- ss.traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.Null
              )
            )
          ).asRight
      )

  test("not on slit"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <-
          createObservationWithModeAs(
            pi,
            p,
            List(t),
            s"""
              flamingos2LongSlit: {
                disperser: R1200_JH
                filter: JH
                fpu: LONG_SLIT_1
                explicitOffsets: [
                  {
                    p: { arcseconds:  60 }
                    q: { arcseconds:   0 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds: 100 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds: 100 }
                  },
                  {
                    p: { arcseconds: 60 }
                    q: { arcseconds:  0 }
                  }
                ]
              }
            """
          )
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: At least one exposure must be taken on slit."
          ).asLeft
      )
