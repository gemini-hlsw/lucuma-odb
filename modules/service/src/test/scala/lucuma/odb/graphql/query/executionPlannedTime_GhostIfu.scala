// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation


class executionPlannedTime_GhostIfu extends ExecutionTestSupportForGhost:
  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  val GhostIfuInput: String = s"""
    ghostIfu: {
      stepCount: 2
      resolutionMode: STANDARD
      red: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 10.0 }
            count: 2
            at: { nanometers: 500 }
          }
        }
        explicitBinning: ONE_BY_TWO
        explicitReadMode: SLOW
      }
      blue: {
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: 5.0 }
            count: 5
            at: { nanometers: 500 }
          }
        }
        explicitBinning: TWO_BY_TWO
        explicitReadMode: MEDIUM
      }
    }
  """

  test("planned time: config and detector estimates"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), GhostIfuInput)
      yield o

    case class Expected(
      color:    String,
      exposure: BigDecimal,
      readout:  BigDecimal,
      count:    Int
    ):
      def datasetEstimate: BigDecimal =
        exposure + readout + BigDecimal(5)

      def totalEstimate: BigDecimal =
        datasetEstimate * count

    def detector(
      red:  Expected,
      blue: Expected
    ): Json =
      def single(d: Expected): Json =
        json"""
          {
            "name": ${s"GHOST ${d.color}".asJson},
            "description": ${s"GHOST ${d.color} Detector Array".asJson},
            "dataset": {
              "estimate": {
                "seconds": ${d.datasetEstimate.asJson}
              },
              "exposure": {
                "seconds": ${d.exposure.asJson}
              },
              "readout": {
                "seconds": ${d.readout.asJson}
              },
              "write": {
                "seconds": 5.000000
              }
            },
            "count": ${d.count.asJson},
            "estimate": {
              "seconds": ${d.totalEstimate.asJson}
            }
          }
        """

      val redJson      = single(red)
      val blueJson     = single(blue)
      val selected     = if red.totalEstimate >= blue.totalEstimate then red else blue
      val selectedJson = single(selected)

      json"""
        {
          "selected": $selectedJson,
          "all": [
            $redJson,
            $blueJson
          ],
          "estimate": {
            "seconds": ${selected.totalEstimate.asJson}
          }
        }
      """

    // slow 1x2 red => 49.6 seconds
    val Red:  Expected = Expected("Red", "10.0".sec, "49.6".sec, 2)

    // medium 2x2 blue => 10.1 seconds
    val Blue: Expected = Expected("Blue", "5.0".sec, "10.1".sec, 5)

    val Step1: Json =
      json"""
        {
          "estimate": {
            "configChange": null,
            "detector": ${detector(Red, Blue)},
            "total": {
              "seconds": ${Red.totalEstimate.asJson}
            }
          }
        }
      """

    val Step2: Json =
      json"""
        {
          "estimate" : {
            "configChange" : null,
            "detector" : ${detector(Red, Blue)},
            "total" : {
              "seconds" : ${Red.totalEstimate.asJson}
            }
          }
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             fragment configChangeEstimateFields on ConfigChangeEstimate {
               name
               description
               estimate {
                 seconds
               }
             }

             fragment allConfigChangeEstimatesFields on AllConfigChangeEstimates {
               selected {
                 ...configChangeEstimateFields
               }
               all {
                 ...configChangeEstimateFields
               }
               estimate {
                 seconds
               }
             }

             fragment datasetEstimateFields on DatasetEstimate {
               estimate {
                 seconds
               }
               exposure {
                 seconds
               }
               readout {
                 seconds
               }
               write {
                 seconds
               }
             }

             fragment detectorEstimateFields on DetectorEstimate {
               name
               description
               dataset {
                 ...datasetEstimateFields
               }
               count
               estimate {
                 seconds
               }
             }

             fragment allDetectorEstimatesFields on AllDetectorEstimates {
               selected {
                 ...detectorEstimateFields
               }
               all {
                 ...detectorEstimateFields
               }
               estimate {
                 seconds
               }
             }

             fragment stepEstimateFields on StepEstimate {
               configChange {
                 ...allConfigChangeEstimatesFields
               }
               detector {
                 ...allDetectorEstimatesFields
               }
               total {
                 seconds
               }
             }

             fragment atomFields on GhostAtom {
               steps {
                 estimate {
                   ...stepEstimateFields
                 }
               }
             }

             query {
               executionConfig(observationId: "$oid") {
                 ghost {
                   science {
                     nextAtom {
                       ...atomFields
                     }
                     possibleFuture {
                       ...atomFields
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "executionConfig": {
                "ghost": {
                  "science": {
                    "nextAtom": {
                       "steps": ${List(Step1).asJson}
                    },
                    "possibleFuture": [
                      {
                        "steps": ${List(Step2).asJson}
                      }
                    ]
                  }
                }
              }
            }
          """.asRight
      )