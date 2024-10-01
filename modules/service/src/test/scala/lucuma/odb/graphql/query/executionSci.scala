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
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given
import lucuma.odb.sequence.gmos.longslit.Science

class executionSci extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  def adjustment(Δλnm: Int, qArcsec: Int): Science.Adjustment =
    Science.Adjustment.apply(
      WavelengthDither.intPicometers.get(Δλnm * 1000),
      Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(qArcsec))
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

  private val ExpectedAfterCalsAndOneScience: Json =
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
    )

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
        expected = ExpectedAfterCalsAndOneScience.asRight
      )
    }
  }

  test("execute arc, flat, one science, then delay") {
    val setup: IO[InstrumentExecutionConfig] =
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
        ic <- generateAfterOrFail(p, o, Science.CalValidityPeriod +| 1.secondTimeSpan)
      } yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map { gn =>
      // We only did one step of (0nm, 0") but we're out of time.  The next
      // atom should be for a (5nm, 15") block.
      assertEquals(gn.nextAtom.description.get, adjustment(5, 15).description)

      // The last atom will be (0nm, 0")
      val lastAtom = gn.possibleFuture.last
      assertEquals(lastAtom.description.get, adjustment(0, 0).description)

      // and it now has 3 science steps left
      val counts = lastAtom.steps.map(_.stepConfig.stepType).groupMapReduce(identity)(_ => 1)
      assertEquals(counts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(counts.get(StepType.Science), 3.some)
    }
  }

  test("order doesn't matter") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
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
        expected = ExpectedAfterCalsAndOneScience.asRight
      )
    }
  }

  test("irrelevant steps may be inserted") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)

        x0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(8), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(x0)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s1)

        x1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(8), scienceStep(10, 10), ObserveClass.Science)
        _  <- addEndStepEvent(x1)

        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s2)

        x2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(8), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(x2)

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
        expected = ExpectedAfterCalsAndOneScience.asRight
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

  def nextAtomId(p: Program.Id, o: Observation.Id): IO[Atom.Id] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(p, o, 5.some).map(_.gmosNorthScience.nextAtom.id)

  test("nextAtom id doesn't change while executing") {
    val setup: IO[(List[Atom.Id], List[Atom.Id])] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        x0 <- nextAtomId(p, o)

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)

        x1 <- nextAtomId(p, o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)

        x2 <- nextAtomId(p, o)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        x3 <- nextAtomId(p, o)

        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        x4 <- nextAtomId(p, o)

        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        x5 <- nextAtomId(p, o)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s5)

        x6 <- nextAtomId(p, o)

      } yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map { (firstAtomIds, secondAtomIds) =>
      assertEquals(firstAtomIds.distinct.length, 1, "first")
      assertEquals(secondAtomIds.distinct.length, 1, "second")
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

  test("duplicate offsets and dithers") {

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

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s0)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s1)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(5), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

      } yield o

    val gcalStepJson: Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          }
        }
      """

    def scienceStepJson(arcsec: Int): Json =
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
          "steps": ${gcalStepJson :: gcalStepJson :: List.fill(scienceSteps)(scienceStepJson(arcsec))}
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
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "description": ${s"5.000 nm, 0.000000″".asJson},
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
              }
            }
          """.asRight
      )
    }
  }

}
