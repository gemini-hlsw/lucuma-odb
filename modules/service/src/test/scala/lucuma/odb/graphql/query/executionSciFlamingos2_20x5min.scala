// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

// Match an example in the F2 Long Slit requirements spreadsheet
class executionSciFlamingos2_20x5min extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 5.minuteTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(20))

  val abba = flamingos2ExpectedScienceAtom(ExposureTime, (0, 15, Enabled), (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled))

  test("simple generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15)),
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )