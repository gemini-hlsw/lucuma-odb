// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given

class executionSci extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  test("simple generation - limited future") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${gmosNorthScienceQuery(1.some)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "gmosNorth" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, p = 0, q = 0, exposures = 3),
                      "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, p = 0, q = 15, exposures = 3)).asJson,
                      "hasMore" -> true.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )
    }

  }

  test("simple generation - unlimited future") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${gmosNorthScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "gmosNorth" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" ->
                        gmosNorthExpectedScienceAtom(ditherNm =  0, p = 0, q =   0, exposures = 3),
                      "possibleFuture" -> List(
                        gmosNorthExpectedScienceAtom(ditherNm =  5, p = 0, q =  15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm = -5, p = 0, q = -15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm =  0, p = 0, q =   0, exposures = 1)
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )
    }
  }

  test("execute arc, flat, one science") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${gmosNorthScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "gmosNorth" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" ->
                        Json.obj(
                          "description" -> s"0.000 nm, 0.000000″".asJson,
                          "observeClass" -> "SCIENCE".asJson,
                          "steps" -> List.fill(2)(gmosNorthExpectedScience(ditherNm = 0, p = 0, q = 0)).asJson
                        ),
                      "possibleFuture" -> List(
                        gmosNorthExpectedScienceAtom(ditherNm =  5, p = 0, q =  15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm = -5, p = 0, q = -15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm =  0, p = 0, q =   0, exposures = 1)
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )
    }
  }

  test("execute arc, flat, one science, fail the flat") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

      } yield o

    // since the atom is still in progress, finish out the science datasets to
    // avoid moving the science fold.  then add the failed flat

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${gmosNorthScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "gmosNorth" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" ->
                        Json.obj(
                          "description" -> s"0.000 nm, 0.000000″".asJson,
                          "observeClass" -> "SCIENCE".asJson,
                          "steps" -> (
                            List.fill(2)(gmosNorthExpectedScience(ditherNm = 0, p = 0, q = 0)) :+
                            gmosNorthExpectedFlat(0)  // repeat the flat
                          ).asJson
                        ),
                      "possibleFuture" -> List(
                        gmosNorthExpectedScienceAtom(ditherNm =  5, p = 0, q =  15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm = -5, p = 0, q = -15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm =  0, p = 0, q =   0, exposures = 1)
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )
    }
  }

  test("execute the first atom, a step of the second, then fail a science dataset from the first") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s5)
        s6 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s6)
        s7 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthScience(5), scienceStep(0, 15), ObserveClass.Science)
        _  <- addEndStepEvent(s7)

        d  <- recordDatasetAs(serviceUser, s2, "N20240905S1001.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

      } yield o

    // since the atom is done, redo the failed step at the end

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${gmosNorthScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "gmosNorth" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" ->
                        Json.obj(
                          "description" -> s"5.000 nm, 15.000000″".asJson,
                          "observeClass" -> "SCIENCE".asJson,
                          "steps" -> (
                            List.fill(2)(gmosNorthExpectedScience(ditherNm = 5, p = 0, q = 15))
                          ).asJson
                        ),
                      "possibleFuture" -> List(
                        gmosNorthExpectedScienceAtom(ditherNm = -5, p = 0, q = -15, exposures = 3),
                        gmosNorthExpectedScienceAtom(ditherNm =  0, p = 0, q =   0, exposures = 2)  // 1 left over, 1 to make up failure
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )
    }
  }

  test("nextAtom id doesn't change while executing") {
    val setup: IO[(List[Atom.Id], List[Atom.Id])] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        x0 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)

        x1 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)

        x2 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        x3 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        x4 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        x5 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s5)

        x6 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

      } yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map { (firstAtomIds, secondAtomIds) =>
      assertEquals(firstAtomIds.distinct.length, 1)
      assertEquals(secondAtomIds.distinct.length, 1)
    }

  }

  test("explicit offsets") {

    val setup: IO[Observation.Id] =
      for {
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
      } yield o

    val gcalStep: Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          }
        }
      """

    def scienceStep(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "SCIENCE",
            "offset": {
              "q": {
                "arcseconds": ${Json.fromBigDecimal(BigDecimal(arcsec).setScale(6))}
              }
            }
          }
        }
      """

    def atom(nm: Int, arcsec: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm, ${BigDecimal(arcsec).setScale(6)}″"
      json"""
        {
          "description": $desc,
          "steps": ${gcalStep :: gcalStep :: List.fill(scienceSteps)(scienceStep(arcsec))}
        }
      """

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     science {
                       offsets {
                         q { arcseconds }
                       }
                     }
                   }
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           description
                           steps {
                             stepConfig {
                               stepType
                               ... on Science {
                                 offset {
                                   q { arcseconds }
                                 }
                               }
                             }
                           }
                         }
                         possibleFuture {
                           description
                           steps {
                             stepConfig {
                               stepType
                               ... on Science {
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
                    "science": {
                      "offsets": [
                        {
                          "q": { "arcseconds": -20.000000 }
                        },
                        {
                          "q": { "arcseconds": 0.000000 }
                        },
                        {
                          "q": { "arcseconds": 20.000000 }
                        }
                      ]
                    }
                  },
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": ${atom(0, -20, 3)},
                        "possibleFuture": ${List(
                          atom( 5,   0, 3),
                          atom(-5,  20, 3),
                          atom( 0, -20, 1)
                        )}
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

  test("explicit wavelength dithers") {

    val setup: IO[Observation.Id] =
      for {
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
      } yield o

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

    def atom(nm: Int, arcsec: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm, ${BigDecimal(arcsec).setScale(6)}″"
      json"""
        {
          "description": $desc,
          "steps": ${List.fill(scienceSteps + 2)(step(nm))}
        }
      """

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
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
                      "science": {
                        "nextAtom": ${atom(-7, 0, 3)},
                        "possibleFuture": ${List(
                          atom(0,  15, 3),
                          atom(7, -15, 3),
                          atom(-7,  0, 1)
                        )}
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

  test("select min x-binning") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t0 <- createTargetWithGaussianAs(pi, p, Angle.fromMicroarcseconds(647_200L))  // X-binning of 4
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
      } yield o

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

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
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
                      "science": {
                        "nextAtom": {
                          "steps": ${List.fill(5)(step)}
                        }
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
