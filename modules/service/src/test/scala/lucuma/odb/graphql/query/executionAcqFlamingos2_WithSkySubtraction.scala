// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

class executionAcqFlamingos2_WithSkySubtraction extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 3.secTimeSpan

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(1))

  val InitialAcquisition: Json =
    json"""
      {
        "executionConfig": {
          "flamingos2": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0, 10)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0,  0)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10,  0)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0, 10)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${flamingos2ExpectedAcq(Flamingos2AcqSlit, ExposureTime, 0, 0)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  test("initial generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )
