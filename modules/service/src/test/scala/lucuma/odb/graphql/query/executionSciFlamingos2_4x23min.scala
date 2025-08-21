// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

// 23 minutes x 4 is greater than the max cycle length
class executionSciFlamingos2_4x23min extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 23.minuteTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("cycle duration too long"):
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
        expected = List(
          s"Could not generate a sequence for $oid: Estimated ABBA cycle time (94.3578125 minutes) for o-100 must be less than 90.000000 minutes."
        ).asLeft
      )
