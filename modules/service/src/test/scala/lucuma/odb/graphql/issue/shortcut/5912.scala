// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.issue.shortcut

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.Observation
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.ItcVersions
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.TargetIntegrationTimeOutcome
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2

class ShortCut_5912 extends ExecutionTestSupportForFlamingos2:

  override protected def itcClient: ItcClient[IO] =
    new ItcClient[IO]:
      override def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        ClientCalculationResult(
          FakeItcVersions,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  // Simulate a "source too bright result"
                  Error.SourceTooBright(BigDecimal(1.0)).asLeft
                )
              )
            ).get
          )
        ).pure[IO]

      override def spectroscopy(
        input: SpectroscopyInput,
        useCache: Boolean
      ): IO[ClientCalculationResult] =
        ClientCalculationResult(
          FakeItcVersions,
          AsterismIntegrationTimeOutcomes(
            NonEmptyChain.fromSeq(
              List.fill(input.asterism.length)(
                TargetIntegrationTimeOutcome(
                  TargetIntegrationTime(Zipper.one(fakeItcSpectroscopyResult), FakeBandOrLine, fakeSignalToNoiseAt(Flamingos2Filter.JH.wavelength).some, Nil).asRight
                )
              )
            ).get
          )
        ).pure[IO]

      def spectroscopyGraphs(
        input: lucuma.itc.client.SpectroscopyGraphsInput,
        useCache: Boolean
      ): IO[lucuma.itc.client.SpectroscopyGraphsResult] =
        IO.raiseError(new java.lang.RuntimeException("spectroscopyGraph: not implemented"))

      def spectroscopyIntegrationTimeAndGraphs(
        input:    lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput,
        useCache: Boolean = true
      ): IO[lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult] =
        IO.raiseError(new java.lang.RuntimeException("spectroscopyIntegrationTimeAndGraph: not implemented"))

      override def versions: IO[ItcVersions] =
        FakeItcVersions.pure[IO]

  test("source too bright"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
            query {
              observation(observationId: "$oid") {
                execution {
                  config {
                    flamingos2 {
                      acquisition {
                        nextAtom {
                          steps {
                            instrumentConfig {
                              exposure { seconds }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          """,
        // expect the first step to be 2 seconds regardless of the source too
        // bright result
        expected =
          json"""
            {
              "observation" : {
                "execution" : {
                  "config" : {
                    "flamingos2" : {
                      "acquisition" : {
                        "nextAtom" : {
                          "steps" : [
                            {
                              "instrumentConfig" : {
                                "exposure" : {
                                  "seconds" : 2.000000
                                }
                              }
                            },
                            {
                              "instrumentConfig" : {
                                "exposure" : {
                                  "seconds" : 10.000000
                                }
                              }
                            },
                            {
                              "instrumentConfig" : {
                                "exposure" : {
                                  "seconds" : 2.000000
                                }
                              }
                            }
                          ]
                        }
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
