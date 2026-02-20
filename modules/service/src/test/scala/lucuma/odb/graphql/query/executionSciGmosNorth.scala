// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionSciGmosNorth extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  extension (Δλnm: Int)
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλnm}.000 nm")

  test("simple generation - limited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("simple generation with blind offset target uses regular targets for science sequence"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        bt <- createTargetWithProfileAs(pi, p)
        o  <- createObservationWithBlindOffset(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("simple generation - unlimited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" ->
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0, 15, -15),
                  "possibleFuture" -> List(
                    gmosNorthExpectedScienceAtom(ditherNm =  5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0)
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  private val ExpectedAfterCalsAndOneScience: Json =
    Json.obj(
      "executionConfig" -> Json.obj(
        "gmosNorth" -> Json.obj(
          "science" -> Json.obj(
            "nextAtom" ->
              gmosNorthExpectedScienceAtom(
                ditherNm = 0,
                List(15, -15).map(q => gmosNorthExpectedScience(0, 0, q))
              ),
            "possibleFuture" -> List(
              gmosNorthExpectedScienceAtom(ditherNm =  5, 0, 15, -15),
              gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
              gmosNorthExpectedScienceAtom(ditherNm =  0, 0)
            ).asJson,
            "hasMore" -> false.asJson
          )
        )
      )
    )

  test("execute arc, flat, one science"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)
       // Arc, flat, one science
        _  <- ss.take(3).traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = ExpectedAfterCalsAndOneScience.asRight
      )

  test("we can start anywhere"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // We do the whole second block, 5 nm wavelength dither
        is <- scienceSequenceIds(serviceUser, o)
        ss  = is.tail.head._2  // second atom steps
       // Arc, flat, one science
        _  <- ss.traverse(sid => addEndStepEvent(sid, v))
        ic <- generateOrFailAs(serviceUser, o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      assertEquals(
        gn.nextAtom.description.get :: gn.possibleFuture.map(_.description.get),
        List(
           0.description,
          -5.description,
           0.description
        )
      )

      def scienceCount(a: Atom[DynamicConfig.GmosNorth]): Int =
        (a.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)).get(StepType.Science).get

      assertEquals(
        scienceCount(gn.nextAtom) :: gn.possibleFuture.map(scienceCount),
        List(3, 3, 1)
      )

  test("order doesn't matter"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        is <- firstScienceAtomStepIds(serviceUser, o)
        _  <- List(is(2), is(0), is(1)).traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = ExpectedAfterCalsAndOneScience.asRight
      )

  test("nextAtom id doesn't change while executing"):
    val setup: IO[(List[Atom.Id], List[Atom.Id])] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        x0 <- firstScienceAtomId(serviceUser, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // First atom
        _  <- addEndStepEvent(ss(0), v)
        x1 <- firstScienceAtomId(serviceUser, o)
        _  <- addEndStepEvent(ss(1), v)
        x2 <- firstScienceAtomId(serviceUser, o)
        _  <- addEndStepEvent(ss(2), v)
        x3 <- firstScienceAtomId(serviceUser, o)
        _  <- addEndStepEvent(ss(3), v)
        x4 <- firstScienceAtomId(serviceUser, o)
        _  <- addEndStepEvent(ss(4), v)

        // We finished the first atom, so we expect a new first science atom here
        x5 <- firstScienceAtomId(serviceUser, o)

        // Second atom, first step
        ssʹ <- firstScienceAtomStepIds(serviceUser, o)
        _   <- addEndStepEvent(ssʹ(0), v)

        x6 <- firstScienceAtomId(serviceUser, o)
      yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map: (firstAtomIds, secondAtomIds) =>
      assertEquals(firstAtomIds.distinct.length, 1, "first")
      assertEquals(secondAtomIds.distinct.length, 1, "second")

  test("explicit offsets"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitSpatialOffsets: [
                { arcseconds: -20.0 },
                { arcseconds:   0.0 },
                { arcseconds:  20.0 }
              ]
            }
          """
        )
        _ <- runObscalcUpdate(p, o)
      yield o

    def telescopeConfig(arcsec: Int): Json =
      json"""
        {
          "offset": {
            "q": {
              "arcseconds": ${Json.fromBigDecimal(BigDecimal(arcsec).setScale(6))}
            }
          }
        }
      """

    def gcalStep(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          },
          "telescopeConfig": ${telescopeConfig(arcsec)}
        }
      """

    def scienceStep(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "SCIENCE"
          },
          "telescopeConfig": ${telescopeConfig(arcsec)}
        }
      """

    def atom(nm: Int, o0: Int, os: Int*): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${gcalStep(o0) :: gcalStep(o0) :: (o0 :: os.toList).map(scienceStep)}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     value {
                       science {
                         telescopeConfigs {
                           offset { q { arcseconds } }
                           guiding
                         }
                       }
                     }
                   }
                 }
               }
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
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
              "observation": {
                "execution": {
                  "digest": {
                    "value": {
                      "science": {
                        "telescopeConfigs": [
                          {
                            "offset": { "q": { "arcseconds": -20.000000 } },
                            "guiding": "ENABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": -20.000000 } },
                            "guiding": "DISABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": 0.000000 } },
                            "guiding": "ENABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": 20.000000 } },
                            "guiding": "ENABLED"
                          }
                        ]
                      }
                    }
                  }
                }
              },
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": ${atom(0, -20, 0, 20)},
                    "possibleFuture": ${List(
                      atom( 5, -20, 0, 20),
                      atom(-5, -20, 0, 20),
                      atom( 0, -20)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  test("explicit wavelength dithers"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitWavelengthDithers: [
                { nanometers: -7.0 },
                { nanometers:  0.0 },
                { nanometers:  7.0 }
              ]
            }
          """
        )
      yield o

    def step(nm: Int): Json =
      json"""
        {
          "instrumentConfig": {
            "gratingConfig": {
              "wavelength": {
                "nanometers": ${Json.fromBigDecimal(BigDecimal(500 + nm).setScale(3))}
              }
            }
          }
        }
      """

    def atom(nm: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${List.fill(scienceSteps + 2)(step(nm))}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         instrumentConfig {
                           gratingConfig {
                             wavelength { nanometers }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         instrumentConfig {
                           gratingConfig {
                             wavelength { nanometers }
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
                    "nextAtom": ${atom(-7, 3)},
                    "possibleFuture": ${List(
                      atom( 0,  3),
                      atom( 7,  3),
                      atom(-7,  1)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  test("select min x-binning"):
    val gaussianProfile = gaussianBandNormalizedProfile(Angle.fromMicroarcseconds(647_200L))
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t0 <- createTargetWithProfileAs(pi, p, gaussianProfile)  // X-binning of 4
        t1 <- createTargetWithProfileAs(pi, p)  // X-binning of 1
        o  <- createObservationWithModeAs(pi, p, List(t0, t1),
               // use a 5" slit so that won't be a factor
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_5_00,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitYBin: TWO
                 }
               """
             )
      yield o

    val step: Json =
      json"""
        {
          "instrumentConfig": {
            "readout": {
              "xBin": "ONE",
              "yBin": "TWO"
            }
          }
        }
      """

    setup.flatMap: oid =>
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
                         instrumentConfig {
                           readout {
                             xBin
                             yBin
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
                      "steps": ${List.fill(5)(step)}
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("duplicate offsets and dithers"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitSpatialOffsets: [
                { arcseconds: 0.0 },
                { arcseconds: 0.0 },
                { arcseconds: 0.0 }
              ],
              explicitWavelengthDithers: [
                { nanometers: 5.0 },
                { nanometers: 5.0 },
                { nanometers: 5.0 }
              ]
            }
          """
        )
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)
        _  <- ss.take(3).traverse(sid => addEndStepEvent(sid, v))
      yield o

    def telescopeConfigJson(arcsec: Int): Json =
      json"""
        {
          "offset": {
            "q": {
              "arcseconds": ${Json.fromBigDecimal(BigDecimal(arcsec).setScale(6))}
            }
          }
        }
      """

    def gcalStepJson(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          },
          "telescopeConfig": ${telescopeConfigJson(arcsec)}
        }
      """

    def scienceStepJson(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "SCIENCE"
          },
          "telescopeConfig": ${telescopeConfigJson(arcsec)}
        }
      """

    def atom(nm: Int, arcsec: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${gcalStepJson(arcsec) :: gcalStepJson(arcsec) :: List.fill(scienceSteps)(scienceStepJson(arcsec))}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
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
                      "description": ${s"5.000 nm".asJson},
                      "steps": ${List.fill(2)(scienceStepJson(0))}
                    },
                    "possibleFuture": ${List(
                      atom(5, 0, 3),
                      atom(5, 0, 3),
                      atom(5, 0, 1)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  test("acquisition step ids do not change while executing science"):
    val execSci: IO[Set[Step.Id]] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        ss <- firstScienceAtomStepIds(serviceUser, o)
        x0 <- firstAcquisitionStepId(serviceUser, o)

        _  <- addEndStepEvent(ss(0), v)
        x1 <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(ss(1), v)
        x2 <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(ss(2), v)
        x3 <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(ss(3), v)
        x4 <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(ss(4), v)
        x5 <- firstAcquisitionStepId(serviceUser, o)

        ssʹ <- firstScienceAtomStepIds(serviceUser, o)
        _  <- addEndStepEvent(ssʹ(0), v)

        x5 <- firstAcquisitionStepId(serviceUser, o)
      yield Set(x0, x1, x2, x3, x4, x5)

    assertIO(execSci.map(_.size), 1)