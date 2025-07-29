// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime


class executionPlannedTime extends ExecutionTestSupportForGmos {
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  test("planned time: config and detector estimates") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    def instrumentConfig(x: GmosXBinning, y: GmosYBinning, r: GmosRoi): Json =
      json"""
        {
          "readout" : {
            "xBin" : ${x.tag.toScreamingSnakeCase.asJson},
            "yBin" : ${y.tag.toScreamingSnakeCase.asJson},
            "ampCount" : "TWELVE",
            "ampGain" : "LOW",
            "ampReadMode" : "FAST"
          },
          "roi" : ${r.tag.toScreamingSnakeCase.asJson}
        }
      """

    def detector(exposure: BigDecimal, readout: BigDecimal): Json =
      val total: BigDecimal = exposure + readout + 10

      val single: Json =
        json"""
          {
            "name": "GMOS North",
            "description": "GMOS North Hamamatsu Detector Array",
            "dataset": {
              "estimate": {
                "seconds": ${total.asJson}
              },
              "exposure": {
                "seconds": ${exposure.asJson}
              },
              "readout": {
                "seconds": ${readout.asJson}
              },
              "write": {
                "seconds": 10.000000
              }
            },
            "count": 1,
            "estimate": {
              "seconds": ${total.asJson}
            }
          }
        """

      json"""
        {
          "selected": $single,
          "all": [
            $single
          ],
          "estimate": {
            "seconds": ${total.asJson}
          }
        }
      """

    val Step1: Json =
      json"""
        {
          "instrumentConfig" : ${instrumentConfig(GmosXBinning.Two, GmosYBinning.Two, GmosRoi.Ccd2)},
          "estimate": {
            "configChange": null,
            "detector": ${detector("10.0".sec, "9.7".sec)},
            "total": {
              "seconds": 29.700000
            }
          }
        }
      """

    val Step2: Json =
      json"""
        {
          "instrumentConfig" : ${instrumentConfig(GmosXBinning.One, GmosYBinning.One, GmosRoi.CentralStamp)},
          "estimate" : {
            "configChange" : {
              "selected" : {
                "name" : "GMOS North FPU",
                "description" : "GMOS North FPU change cost",
                "estimate" : {
                  "seconds" : 60.000000
                }
              },
              "all" : [
                {
                  "name" : "GMOS North FPU",
                  "description" : "GMOS North FPU change cost",
                  "estimate" : {
                    "seconds" : 60.000000
                  }
                },
                {
                  "name" : "Offset",
                  "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                  "estimate" : {
                    "seconds" : 7.062500
                  }
                }
              ],
              "estimate" : {
                "seconds" : 60.000000
              }
            },
            "detector" : ${detector("20".sec, "4.2".sec)},
            "total" : {
              "seconds" : 94.200000
            }
          }
        }
      """

    val Step3: Json =
      json"""
        {
          "instrumentConfig" : ${instrumentConfig(GmosXBinning.One, GmosYBinning.One, GmosRoi.CentralStamp)},
          "estimate" : {
            "configChange" : {
              "selected" : {
                "name" : "Offset",
                "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                "estimate" : {
                  "seconds" : 7.062500
                }
              },
              "all" : [
                {
                  "name" : "Offset",
                  "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                  "estimate" : {
                    "seconds" : 7.062500
                  }
                }
              ],
              "estimate" : {
                "seconds" : 7.062500
              }
            },
            "detector" : ${detector("30".sec, "4.2".sec)},
            "total" : {
              "seconds" : 51.262500
            }
          }
        }
      """

    val Step4: Json =
      json"""
        {
          "instrumentConfig" : ${instrumentConfig(GmosXBinning.One, GmosYBinning.One, GmosRoi.CentralStamp)},
          "estimate" : {
            "configChange" : null,
            "detector" : ${detector("30".sec, "4.2".sec)},
            "total" : {
              "seconds" : 44.200000
            }
          }
        }
      """

    setup.flatMap { oid =>
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

             fragment gmosNorthAtomFields on GmosNorthAtom {
               steps {
                 instrumentConfig {
                   readout {
                     xBin
                     yBin
                     ampCount
                     ampGain
                     ampReadMode
                   }
                   roi
                 }
                 estimate {
                   ...stepEstimateFields
                 }
               }
             }

             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       acquisition {
                         nextAtom {
                           ...gmosNorthAtomFields
                         }
                         possibleFuture {
                           ...gmosNorthAtomFields
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "acquisition": {
                        "nextAtom": {
                           "steps": ${List(Step1, Step2, Step3).asJson}
                        },
                        "possibleFuture": [
                          {
                            "steps": ${List(Step4).asJson}
                          }
                        ]
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

  test("planned time: observation level") {

    // * Arc:
    //   * Science Fold.: 15.0
    //   * Exposure Time:  1.0
    //   * Readout......: 41.1
    //   * Writeout.....: 10.0
    //                    ----
    //                    67.1

    // * Flat:
    //   * GCal Change..:  5.0
    //   * Exposure Time:  1.0
    //   * Readout......: 41.1
    //   * Writeout.....: 10.0
    //                    ----
    //                    57.1

    // * Science (1):
    // (there is an offset cost for some steps but it is subsumed by the
    //  science fold movement)
    //   * Science Fold:    15.0
    //   * Exposure Time: 1200.0
    //   * Readout......:   41.1
    //   * Writeout.....:   10.0
    //                      ----
    //                    1266.1

    // * Science (2 and 3):
    //   * Exposure Time: 1200.0
    //   * Readout......:   41.1
    //   * Writeout.....:   10.0
    //                      ----
    //                    1251.1

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    def step(time: String): Json =
      json"""
        {
          "estimate" : {
            "total" : {
              "seconds" : ${time.sec.asJson}
            }
          }
        }
      """

    def atom(scienceObserves: Int): Json =
      json"""
        {
          "steps" : ${
            (List(step("67.1"), step("57.1")) ++ (
              if scienceObserves > 0 then
                step("1266.1") :: List.fill(scienceObserves - 1)(step("1251.1"))
              else
                Nil
            )).asJson
          }
        }
      """

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             fragment stepEstimateFields on StepEstimate {
               total {
                 seconds
               }
             }

             fragment gmosNorthAtomFields on GmosNorthAtom {
               steps {
                 estimate {
                   ...stepEstimateFields
                 }
               }
             }

             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           ...gmosNorthAtomFields
                         }
                         possibleFuture {
                           ...gmosNorthAtomFields
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config" : {
                    "gmosNorth": {
                      "science" : {
                        "nextAtom" : ${atom(3)},
                        "possibleFuture" : [
                          ${atom(3)},
                          ${atom(3)},
                          ${atom(1)}
                        ]
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

}
