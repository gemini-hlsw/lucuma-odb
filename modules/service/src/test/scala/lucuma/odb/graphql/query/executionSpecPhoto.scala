// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProposalStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculationState
import lucuma.itc.IntegrationTime

class executionSpecPhoto extends ExecutionTestSupportForGmos {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  // A spec photo in a program whose proposal has yet to be accepted.
  private val proposalStageSpecPhoto: IO[(Program.Id, Observation.Id)] =
    for {
      c <- createGeminiCallForProposalsAs(staff)
      p <- createProgram
      _ <- addQueueProposal(pi, p, c)
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- setObservationCalibrationRole(List(o), CalibrationRole.SpectroPhotometric)
    } yield (p, o)

  private def scienceTimeEstimate(oid: Observation.Id): IO[BigDecimal] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value { science { timeEstimate { total { seconds } } } }
              }
            }
          }
        }
      """
    ).map: json =>
      json.hcursor
        .downFields("observation", "execution", "digest", "value", "science", "timeEstimate", "total", "seconds")
        .require[BigDecimal]

  // Total time for the observation, setup included.
  private def totalTimeEstimate(oid: Observation.Id): IO[BigDecimal] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value { estimate { total { total { seconds } } } }
              }
            }
          }
        }
      """
    ).map: json =>
      json.hcursor
        .downFields("observation", "execution", "digest", "value", "estimate", "total", "total", "seconds")
        .require[BigDecimal]

  // The science sequence, absent when none was produced for the observation.
  private def scienceSequence(oid: Observation.Id): IO[Option[Json]] =
    query(
      user  = pi,
      query = s"""
        query {
          executionConfig(observationId: "$oid") {
            gmosNorth {
              science { nextAtom { steps { instrumentConfig { exposure { seconds } } } } }
            }
          }
        }
      """
    ).map: json =>
      json.hcursor
        .downFields("executionConfig", "gmosNorth", "science")
        .require[Option[Json]]

  test("spec photo - a waiting proposal is charged a flat 20 minutes") {
    proposalStageSpecPhoto.flatMap { (pid, oid) =>
      for {
        _   <- runObscalcUpdate(pid, oid)
        tot <- totalTimeEstimate(oid)
        sci <- scienceTimeEstimate(oid)
      } yield
        assertEquals(tot, BigDecimal("1200.000000"))
        assertEquals(sci, BigDecimal("1200.000000"), "the whole charge is science; there is no setup to add")
    }
  }

  // Nothing to disagree with the 20 minutes: which star the standard uses, and
  // so what it really costs, is not settled until the proposal is accepted.
  test("spec photo - a waiting proposal has no sequence") {
    proposalStageSpecPhoto.flatMap { (pid, oid) =>
      for {
        _ <- runObscalcUpdate(pid, oid)
        a <- scienceSequence(oid)
      } yield assertEquals(a, None, "a waiting standard should produce no sequence")
    }
  }

  // Nothing is generated for a waiting standard, so nothing the generator needs
  // may be required of it either.  An incomplete target defeats the ITC, and the
  // placeholder still has to come back rather than an error.
  test("spec photo - a waiting proposal needs no ITC result") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        c <- createGeminiCallForProposalsAs(staff)
        p <- createProgram
        _ <- addQueueProposal(pi, p, c)
        t <- createIncompleteTargetAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.SpectroPhotometric)
      } yield (p, o)

    setup.flatMap { (pid, oid) =>
      for {
        _   <- runObscalcUpdate(pid, oid)
        tot <- totalTimeEstimate(oid)
        a   <- scienceSequence(oid)
      } yield
        assertEquals(tot, BigDecimal("1200.000000"))
        assertEquals(a, None, "the placeholder should not depend on an ITC result")
    }
  }

  test("spec photo - accepted proposal gets a real sequence and estimate") {
    proposalStageSpecPhoto.flatMap { (pid, oid) =>
      for {
        _   <- setProposalStatusDirectly(pid, ProposalStatus.Accepted)
        _   <- runObscalcUpdate(pid, oid)
        tot <- totalTimeEstimate(oid)
        a   <- scienceSequence(oid)
      } yield
        assertEquals(tot, BigDecimal("2293.200000"))
        assert(a.isDefined, "an accepted standard should produce a sequence")
    }
  }

  test("spec photo - accepting the proposal invalidates the stored estimate") {
    proposalStageSpecPhoto.flatMap { (pid, oid) =>
      for {
        _  <- runObscalcUpdate(pid, oid)
        t0 <- totalTimeEstimate(oid)
        s0 <- selectCalculationStates.map(_.get(oid))
        _  <- setProposalStatusDirectly(pid, ProposalStatus.Accepted)
        s1 <- selectCalculationStates.map(_.get(oid))
        _  <- runObscalcUpdate(pid, oid)
        t1 <- totalTimeEstimate(oid)
      } yield
        assertEquals(t0, BigDecimal("1200.000000"))
        assertEquals(s0, Some(CalculationState.Ready))
        assertEquals(s1, Some(CalculationState.Pending), "accepting the proposal should mark the spec photo for recalculation")
        assertEquals(t1, BigDecimal("2293.200000"))
    }
  }

  test("spec photo") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.SpectroPhotometric)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       observeClass
                       steps {
                         observeClass
                         instrumentConfig {
                           exposure {
                             seconds
                           }
                           readout {
                             xBin
                             yBin
                             ampCount
                             ampGain
                             ampReadMode
                           }
                           dtax
                           roi
                           gratingConfig {
                             grating
                             order
                             wavelength {
                               nanometers
                             }
                           }
                           filter
                           fpu {
                             builtin
                             customMask { slitWidth }
                           }
                         }
                         telescopeConfig {
                           offset {
                             p { arcseconds }
                             q { arcseconds }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       steps {
                         instrumentConfig {
                           exposure {
                             seconds
                           }
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
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": {
                      "observeClass": "NIGHT_CAL",
                      "steps": [
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 1.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "TWO",
                              "ampCount": "TWELVE",
                              "ampGain": "LOW",
                              "ampReadMode": "SLOW"
                            },
                            "dtax": "ZERO",
                            "roi": "FULL_FRAME",
                            "gratingConfig": {
                              "grating" : "R831_G5302",
                              "order" : "ONE",
                              "wavelength" : {
                                "nanometers" : 500.000
                              }
                            },
                            "filter": "R_PRIME",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50",
                              "customMask": null
                            }
                          },
                          "telescopeConfig": {
                            "offset": {
                              "p": {
                                "arcseconds": 0.000000
                              },
                              "q": {
                                "arcseconds": 0.000000
                              }
                            }
                          }
                        },
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 1200.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "TWO",
                              "ampCount": "TWELVE",
                              "ampGain": "LOW",
                              "ampReadMode": "SLOW"
                            },
                            "dtax": "ZERO",
                            "roi": "FULL_FRAME",
                            "gratingConfig": {
                              "grating": "R831_G5302",
                              "order": "ONE",
                              "wavelength": {
                                "nanometers": 500.000
                              }
                            },
                            "filter": "R_PRIME",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50",
                              "customMask": null
                            }
                          },
                          "telescopeConfig": {
                            "offset": {
                              "p": {
                                "arcseconds": 0.000000
                              },
                              "q": {
                                "arcseconds": 0.000000
                              }
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": []
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

  test("spec photo, small custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither up to 23.5 nm is ignored
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 10.0
                     },
                     {
                       nanometers: 23.5
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.SpectroPhotometric)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       steps {
                         observeClass
                         instrumentConfig {
                           gratingConfig {
                             wavelength {
                               nanometers
                             }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       steps {
                         observeClass
                         instrumentConfig {
                           gratingConfig {
                             wavelength {
                               nanometers
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
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength" : {
                                "nanometers" : 500.000
                              }
                            }
                          }
                        },
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength": {
                                "nanometers": 500.000
                              }
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": []
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

  test("spec photo, large custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither over 23.5 nm is tracked
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 23.501
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.SpectroPhotometric)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       steps {
                         observeClass
                         instrumentConfig {
                           gratingConfig {
                             wavelength {
                               nanometers
                             }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       steps {
                         observeClass
                         instrumentConfig {
                           gratingConfig {
                             wavelength {
                               nanometers
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
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength" : {
                                "nanometers" : 500.000
                              }
                            }
                          }
                        },
                        {
                          "observeClass": "NIGHT_CAL",
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength": {
                                "nanometers": 500.000
                              }
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": [
                      {
                        "steps": [
                          {
                            "observeClass": "NIGHT_CAL",
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength" : {
                                  "nanometers" : 523.501
                                }
                              }
                            }
                          },
                          {
                            "observeClass": "NIGHT_CAL",
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 523.501
                                }
                              }
                            }
                          }
                        ]
                      }
                    ]
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

}
