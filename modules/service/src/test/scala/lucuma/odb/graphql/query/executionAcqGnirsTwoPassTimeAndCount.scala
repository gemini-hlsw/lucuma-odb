// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.literal.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.odb.sequence.gnirs.AcquisitionClassificationSignalToNoise

// Exercises the time-and-count two-pass acquisition ITC (both acquisition mode and
// filter auto). The classification pass must run at the fixed classification S/N even
// though the user's acquisition ETM is time-and-count — otherwise the acquisition type
// leaks from the user's exposure (the reported bug).
class executionAcqGnirsTwoPassTimeAndCount extends ExecutionTestSupportForGnirs:

  // Imaging ITC:
  //   - classification pass (fixed S/N)  → 0.3s → Very Bright
  //   - real pass (user's time-and-count) → echoes the user's 2s / 3 count
  // The 2s real exposure is deliberately in the Bright range: if the sequence re-derived
  // the mode from it (the bug) it would pick the broadband filter, so seeing H2 proves
  // the mode was pinned to Very Bright from the classification pass.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(ExposureTimeMode.SignalToNoiseMode(sn, _), _, _, _, _, _, _) =>
        // Only the classification pass runs in S/N mode here; it uses the classification S/N.
        if sn === AcquisitionClassificationSignalToNoise then IntegrationTime(300.msTimeSpan, PosInt.unsafeFrom(1)).some
        else                                                  none
      case InstrumentMode.GnirsImaging(ExposureTimeMode.TimeAndCountMode(time, count, _), _, _, _, _, _, _) =>
        IntegrationTime(time, count).some
      case _                                                                                        =>
        none

  test("time-and-count mode auto acquisition: classification pins Very Bright, real exposure uses H2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        // Time-and-count mode, acquisition mode and filter left auto ⇒ two-pass.
        _ <- setAcquisitionTimeAndCount(o, 2, 3, 1645)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gnirs {
                acquisition {
                  nextAtom {
                    steps {
                      instrumentConfig { exposure { microseconds } filter }
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      { "instrumentConfig": { "exposure": { "microseconds": 3000000 }, "filter": "ORDER4" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "filter": "H2" } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )
