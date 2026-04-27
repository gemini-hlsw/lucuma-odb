// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.JsonObject
import lucuma.itc.ItcVersions

object SpectroscopyIntegrationTime extends GraphQLOperation[Unit] {
  type Data      = ClientCalculationResult
  type Variables = SpectroscopyInput

  override val document: String =
    """
      fragment IntegrationTimeFields on IntegrationTime {
        exposureTime {
          microseconds
        }
        exposureCount
      }

      fragment TargetIntegrationTimeFields on TargetIntegrationTime {
        band
        emissionLine { picometers }
        all {
          ...IntegrationTimeFields
        }
        index
        selected {
          ...IntegrationTimeFields
        }
        ccds {
          singleSNRatio
          maxSingleSNRatio
          totalSNRatio
          maxTotalSNRatio
          wavelengthForMaxTotalSNRatio {
            picometers
          }
          wavelengthForMaxSingleSNRatio {
            picometers
          }
          peakPixelFlux
          wellDepth
          ampGain
          warnings {
            msg
          }
        }
        signalToNoiseAt {
          single
          total
          wavelength {
            picometers
          }
        }
      }

      fragment TargetIntegrationTimeOutcomeFields on TargetIntegrationTimeOutcome {
        ... on TargetIntegrationTime {
          ...TargetIntegrationTimeFields
        }
        ... on TargetError {
          errorCode
          message
          wellHalfFilledSeconds
          wavelength { picometers }
        }
      }

      query Spectroscopy($spec: SpectroscopyInput!) {
        spectroscopy(input: $spec) {
          versions {
            serverVersion
            dataVersion
          }
          targetTimes {
            ...TargetIntegrationTimeOutcomeFields
          }
          brightestIndex
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[SpectroscopyInput] { input =>
      JsonObject(
        "spec" -> Encoder[SpectroscopyInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[ClientCalculationResult] =
    (c: HCursor) => c.downField("spectroscopy").as[ClientCalculationResult]
}

object ImagingIntegrationTime extends GraphQLOperation[Unit] {
  type Data      = ClientCalculationResult
  type Variables = ImagingInput

  override val document: String =
    """
      fragment IntegrationTimeFields on IntegrationTime {
        exposureTime {
          microseconds
        }
        exposureCount
      }

      fragment TargetIntegrationTimeFields on TargetIntegrationTime {
        band
        emissionLine { picometers }
        all {
          ...IntegrationTimeFields
        }
        index
        selected {
          ...IntegrationTimeFields
        }
        ccds {
          singleSNRatio
          maxSingleSNRatio
          totalSNRatio
          maxTotalSNRatio
          wavelengthForMaxTotalSNRatio {
            picometers
          }
          wavelengthForMaxSingleSNRatio {
            picometers
          }
          peakPixelFlux
          wellDepth
          ampGain
          warnings {
            msg
          }
        }
        signalToNoiseAt {
          single
          total
          wavelength {
            picometers
          }
        }
      }

      fragment TargetIntegrationTimeOutcomeFields on TargetIntegrationTimeOutcome {
        ... on TargetIntegrationTime {
          ...TargetIntegrationTimeFields
        }
        ... on TargetError {
          errorCode
          message
          wellHalfFilledSeconds
          wavelength { picometers }
        }
      }

      query Imaging($spec: ImagingInput!) {
        imaging(input: $spec) {
          versions {
            serverVersion
            dataVersion
          }
          targetTimes {
            ...TargetIntegrationTimeOutcomeFields
          }
          brightestIndex
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[ImagingInput] { input =>
      JsonObject(
        "spec" -> Encoder[ImagingInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[ClientCalculationResult] =
    (c: HCursor) => c.downField("imaging").as[ClientCalculationResult]
}

object SpectroscopyIntegrationTimeAndGraphsQuery extends GraphQLOperation[Unit] {
  type Data      = SpectroscopyIntegrationTimeAndGraphsResult
  type Variables = SpectroscopyIntegrationTimeAndGraphsInput

  val document =
    """
      fragment IntegrationTimeFields on IntegrationTime {
        exposureTime {
          microseconds
        }
        exposureCount
      }

      fragment TargetIntegrationTimeFields on TargetIntegrationTime {
        band
        emissionLine { picometers }
        all {
          ...IntegrationTimeFields
        }
        index
        selected {
          ...IntegrationTimeFields
        }
      }

      fragment TargetTimeAndGraphsFields on TargetTimeAndGraphs {
        integrationTime {
          ...TargetIntegrationTimeFields
        }
        graphs {
          peakFinalSNRatio
          atWavelengthFinalSNRatio
          peakSingleSNRatio
          atWavelengthSingleSNRatio
          ccds {
            singleSNRatio
            totalSNRatio
            peakPixelFlux
            ampGain
            maxTotalSNRatio
            maxSingleSNRatio
            wavelengthForMaxTotalSNRatio {
              picometers
            }
            wavelengthForMaxSingleSNRatio {
              picometers
            }
            wellDepth
            warnings {
              msg
            }
          }
          graphData {
            graphType
            series {
              title
              seriesType
              dataY
              xAxis {
                start
                end
                count
                min
                max
              }
              yAxis {
                start
                end
                count
                min
                max
              }
            }
          }
        }
      }

      fragment TargetErrorFields on TargetError {
          errorCode
          message
          wellHalfFilledSeconds
          wavelength { picometers }
      }

      fragment TargetTimeAndGraphsOutcomeFields on TargetTimeAndGraphsOutcome {
        ... on TargetTimeAndGraphs {
          ...TargetTimeAndGraphsFields
        }
        ... on TargetError {
          ...TargetErrorFields
        }
      }

      query($input: SpectroscopyIntegrationTimeAndGraphsInput!) {
        spectroscopyIntegrationTimeAndGraphs(input: $input) {
          versions {
            serverVersion
            dataVersion
          }
          targetTimesAndGraphs {
            ...TargetTimeAndGraphsOutcomeFields
          }
          brightestIndex
        }
      }
  """

  override val varEncoder: Encoder.AsObject[Variables] =
    Encoder.AsObject.instance[SpectroscopyIntegrationTimeAndGraphsInput] { input =>
      JsonObject(
        "input" -> Encoder[SpectroscopyIntegrationTimeAndGraphsInput].apply(input)
      )
    }

  override val dataDecoder: Decoder[SpectroscopyIntegrationTimeAndGraphsResult] = c =>
    val result = c.downField("spectroscopyIntegrationTimeAndGraphs")
    for
      versions       <- result.downField("versions").as[ItcVersions]
      graphs         <- result.downField("targetTimesAndGraphs").as[AsterismTimeAndGraphsResult]
      brightestIndex <- result.downField("brightestIndex").as[Option[Int]]
    yield SpectroscopyIntegrationTimeAndGraphsResult(versions, graphs, brightestIndex)
}
