// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.query.ExecutionTestSupportForGnirs

// A GNIRS target the ITC reports as too bright must fall back to the shortest legal
// exposure, the Very Bright read mode minimum (0.2s), not below it.
class ShortCut_10825 extends ExecutionTestSupportForGnirs:

  override protected def itcClient: ItcClient[IO] =
    val base: ItcClient[IO] = super.itcClient
    new ItcClient[IO]:
      override def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        ClientCalculationResult(
          FakeItcVersions,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(Error.SourceTooBright(BigDecimal(0.2)).asLeft)
              )
            ).get
          )
        ).pure[IO]

      override def spectroscopy(input: SpectroscopyInput, useCache: Boolean): IO[ClientCalculationResult] =
        base.spectroscopy(input, useCache)

      override def spectroscopyIntegrationTimeAndGraphs(
        input:    lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): IO[lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult] =
        base.spectroscopyIntegrationTimeAndGraphs(input, useCache)

      override def versions: IO[lucuma.itc.ItcVersions] =
        base.versions

  test("source too bright: acquisition falls back to the Very Bright read mode minimum"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsLongSlitObservationAs(pi, p, t)
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
                      instrumentConfig { exposure { milliseconds } coadds readMode filter }
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
                      { "instrumentConfig": { "exposure": { "milliseconds": 3000.000 }, "coadds": 1,  "readMode": "BRIGHT",      "filter": "ORDER4" } },
                      { "instrumentConfig": { "exposure": { "milliseconds":  200.000 }, "coadds": 25, "readMode": "VERY_BRIGHT", "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "milliseconds":  200.000 }, "coadds": 25, "readMode": "VERY_BRIGHT", "filter": "H2" } },
                      { "instrumentConfig": { "exposure": { "milliseconds":  200.000 }, "coadds": 25, "readMode": "VERY_BRIGHT", "filter": "H2" } }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )
