// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

// Match examples in the F2 Long Slit requirements spreadsheet
class executionSciFlamingos2_4x5min extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 5.minuteTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("(60, 0), (0, 0), (0, 0), (60, 0)"):
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
                    p: { arcseconds: 60 }
                    q: { arcseconds:  0 }
                  },
                  {
                    p: { arcseconds:  0 }
                    q: { arcseconds:  0 }
                  },
                  {
                    p: { arcseconds:  0 }
                    q: { arcseconds:  0 }
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

    // 2 cycles required because each cycle only produces 2 science
    // steps and ITC reports we need 4.
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (60, 0, Disabled), (0, 0, Enabled), (0, 0, Enabled), (60, 0, Disabled)),
                  "possibleFuture" -> List(
                    flamingos2ExpectedScienceAtom(ExposureTime, (60, 0, Disabled), (0, 0, Enabled), (0, 0, Enabled), (60, 0, Disabled)),
                    flamingos2ExpectedGcals((60, 0))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("(300, 55), (0, -5), (0, 5), (300, 65)"):
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
                    p: { arcseconds: 300 }
                    q: { arcseconds:  55 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds:  -5 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds:   5 }
                  },
                  {
                    p: { arcseconds: 300 }
                    q: { arcseconds:  65 }
                  }
                ]
              }
            """
          )
      yield o

    // 2 cycles required because each cycle only produces 2 science
    // steps and ITC reports we need 4.
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (300, 55, Disabled), (0, -5, Enabled), (0, 5, Enabled), (300, 65, Disabled)),
                  "possibleFuture" -> List(
                    flamingos2ExpectedScienceAtom(ExposureTime, (300, 55, Disabled), (0, -5, Enabled), (0, 5, Enabled), (300, 65, Disabled)),
                    flamingos2ExpectedGcals((300, 65))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )
