// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.odb.sequence.gnirs.AcquisitionClassificationSignalToNoise

/**
 * S/N-mode acquisition steps after the FPU image are brought towards a 5s integration:
 * the ITC exposure count is raised to round(5s / exposure). See
 * https://app.shortcut.com/lucuma/story/10747/augment-gnirs-coadding-in-acquisitions.
 */
class executionAcqGnirsIntegrationTarget extends ExecutionTestSupportForGnirs:

  // The classification pass (fixed S/N) answers 0.3s x 1, so every target here is Very
  // Bright (H2). The real pass answers the user's S/N read as milliseconds, with a single
  // exposure, so each test picks its exposure time through the S/N it requests.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(exposureTimeMode = ExposureTimeMode.SignalToNoiseMode(sn, _)) =>
        if sn === AcquisitionClassificationSignalToNoise then
          IntegrationTime(300.msTimeSpan, PosInt.unsafeFrom(1)).some
        else
          IntegrationTime(TimeSpan.unsafeFromMicroseconds(sn.toBigDecimal.toLong * 1_000L), PosInt.unsafeFrom(1)).some
      case _                                                                                        =>
        none

  // Columns: requested S/N (= exposure ms), expected coadds. The FPU image (first step)
  // keeps its fixed 3s single-coadd exposure; the remaining steps carry the raised coadds.
  List(
    (BigDecimal(200),  25),
    (BigDecimal(300),  17),
    (BigDecimal(3300),  2),
    (BigDecimal(3400),  1)
  ).foreach: (sn, coadds) =>
    val expµs: Long = sn.toLong * 1_000L
    test(s"a ${sn}ms single exposure is taken with $coadds coadds"):
      val setup: IO[Observation.Id] =
        for
          p <- createProgram
          t <- createTargetWithProfileAs(pi, p)
          o <- createGnirsLongSlitObservationAs(pi, p, t)
          _ <- setAcquisitionSignalToNoise(o, sn, 1645)
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
                        { "instrumentConfig": { "exposure": { "microseconds": $expµs }, "coadds": $coadds, "filter": "H2" } },
                        { "instrumentConfig": { "exposure": { "microseconds": $expµs }, "coadds": $coadds, "filter": "H2" } },
                        { "instrumentConfig": { "exposure": { "microseconds": $expµs }, "coadds": $coadds, "filter": "H2" } }
                      ]
                    }
                  }
                }
              }
            }
          """.asRight
        )
