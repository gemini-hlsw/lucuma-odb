// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcResultReadMode

// In S/N mode the science steps take the read mode the ITC chose: Faint here, where 20s
// would derive Bright.
class executionSciFlamingos2ReadMode extends ExecutionTestSupportForFlamingos2:
  val ExposureTime: TimeSpan         = 20.secondTimeSpan
  val ItcChoice:    Flamingos2ReadMode = Flamingos2ReadMode.Faint

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4), readMode = ItcResultReadMode.Flamingos2(ItcChoice).some)

  test("science steps take the read mode the ITC chose"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    val abba: Json =
      flamingos2ExpectedScienceAtom(
        flamingos2Science(ExposureTime, ItcChoice),
        (0, 10, Enabled), (0, -10, Enabled), (0, -10, Enabled), (0, 10, Enabled)
      )

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom"       -> flamingos2ExpectedGcals((0, 10)),
                  "possibleFuture" -> List(abba).asJson,
                  "hasMore"        -> false.asJson
                )
              )
            )
          ).asRight
      )
