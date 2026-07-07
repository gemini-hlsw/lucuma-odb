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

// Exercises the S/N-mode two-pass acquisition ITC (both acquisition mode and filter auto).
class executionAcqGnirsTwoPass extends ExecutionTestSupportForGnirs:

  // Imaging ITC keyed on the requested S/N so the two passes return different results:
  //   - classification pass (fixed S/N)  → 0.3s → Very Bright
  //   - real pass          (user's S/N)  → 2s   → would classify as Bright on its own
  // The 2s real exposure is deliberately in the Bright range: if the sequence re-derived
  // the mode from it (the old bug) it would pick the broadband filter, so seeing H2 proves
  // the mode was pinned to Very Bright from the classification pass.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(ExposureTimeMode.SignalToNoiseMode(sn, _), _, _, _, _, _, _) =>
        if sn === AcquisitionClassificationSignalToNoise then IntegrationTime(300.msTimeSpan, PosInt.unsafeFrom(1)).some
        else                                                  IntegrationTime(2.secTimeSpan,  PosInt.unsafeFrom(3)).some
      case _                                                                                        =>
        none

  test("S/N mode auto acquisition: classification pins Very Bright, real exposure uses H2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
        // S/N mode, acquisition mode and filter left auto ⇒ two-pass.
        _ <- setAcquisitionSignalToNoise(o, 6, 1645)
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
                      instrumentConfig { exposure { microseconds } coadds filter }
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
                      { "instrumentConfig": { "exposure": { "microseconds": 3000000 }, "coadds": 1, "filter": "ORDER4" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "coadds": 3, "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "coadds": 3, "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "microseconds": 2000000 }, "coadds": 3, "filter": "H2" } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )
