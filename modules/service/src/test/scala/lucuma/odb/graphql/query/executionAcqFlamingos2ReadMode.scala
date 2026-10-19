// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcReadMode
import lucuma.itc.client.ImagingInput
import lucuma.odb.sequence.flamingos2.longslit.Acquisition.RepeatingAtomCount

// In S/N mode the acquisition steps take the read mode the ITC chose: Medium here,
// where 2s would derive Bright.  The fixed 10s slit image keeps it too.
class executionAcqFlamingos2ReadMode extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan           = 2.secTimeSpan
  val ItcChoice:    Flamingos2ReadMode = Flamingos2ReadMode.Medium

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(t, c, _) => IntegrationTime(t, c).some
      case _                                          => IntegrationTime(ExposureTime, PosInt.unsafeFrom(1), readMode = ItcReadMode.Flamingos2(ItcChoice).some).some

  val AcqImage: Flamingos2DynamicConfig =
    Flamingos2AcqImage.copy(readMode = ItcChoice, reads = ItcChoice.readCount)

  val AcqSlit: Flamingos2DynamicConfig =
    Flamingos2AcqSlit.copy(readMode = ItcChoice, reads = ItcChoice.readCount)

  val fineAdjustments: Json =
    List
      .fill(RepeatingAtomCount):
        json"""
          {
            "description": "Fine Adjustments",
            "observeClass": "ACQUISITION",
            "steps": [ ${flamingos2ExpectedAcq(AcqSlit, ExposureTime, 0, 0)} ]
          }
        """
      .asJson

  test("acquisition steps take the read mode the ITC chose"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    val expected: Json =
      json"""
        {
          "executionConfig": {
            "flamingos2": {
              "acquisition": {
                "nextAtom": {
                  "description": "Initial Acquisition",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${flamingos2ExpectedAcq(AcqImage, ExposureTime,    0,  0)},
                    ${flamingos2ExpectedAcq(AcqSlit,  10.secTimeSpan, 10,  0)},
                    ${flamingos2ExpectedAcq(AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)}
                  ]
                },
                "possibleFuture": ${fineAdjustments},
                "hasMore": false
              }
            }
          }
        }
      """

    setup.flatMap: oid =>
      expect(pi, flamingos2AcquisitionQuery(oid), expected.asRight)
