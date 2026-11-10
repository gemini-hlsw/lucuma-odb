// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.flamingos2.longslit.Acquisition.RepeatingAtomCount

class executionAcqFlamingos2_WithSkySubtraction extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 3.secTimeSpan

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(1))

  private def acquisition(initialSteps: Json*): Json =
    json"""
      {
        "executionConfig": {
          "flamingos2": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": ${initialSteps.toList}
              },
              "possibleFuture": ${
                List
                  .fill(RepeatingAtomCount):
                    json"""
                      {
                        "description": "Fine Adjustments",
                        "observeClass": "ACQUISITION",
                        "steps": [
                          ${flamingos2ExpectedAcq(Flamingos2AcqSlit, ExposureTime, 0, 0)}
                        ]
                      }
                    """
                  .asJson
              },
              "hasMore": false
            }
          }
        }
      }
    """

  private def setup(acquisition: String): IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(
             pi,
             p,
             List(t),
             s"flamingos2LongSlit: { disperser: R1200_JH, filter: JH, fpu: LONG_SLIT_1 $acquisition }"
           )
    yield o

  test("initial generation"):
    setup("").flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = acquisition(
          flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0, 10),
          flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0,  0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10,  0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0, 10),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)
        ).asRight
      )

  test("an explicit BRIGHT acquisition takes no sky steps"):
    setup("acquisition: { explicitAcquisitionType: BRIGHT }").flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = acquisition(
          flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0, 0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10, 0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0, 0, Breakpoint.Enabled)
        ).asRight
      )

  test("an explicit FAINT acquisition takes its sky steps at its sky offset"):
    val faint: String =
      "acquisition: { explicitAcquisitionType: FAINT, skyOffset: { p: { arcseconds: 5 }, q: { arcseconds: 0 } } }"
    setup(faint).flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = acquisition(
          flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    5, 0),
          flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0, 0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10, 0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    5, 0),
          flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0, 0, Breakpoint.Enabled)
        ).asRight
      )
