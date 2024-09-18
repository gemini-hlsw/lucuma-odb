// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
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
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
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
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
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

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
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

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
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

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
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

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 5)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, ObserveClass.PartnerCal)
        _  <- addEndStepEvent(s5)

        x6 <- genGmosNorthSequence(o, SequenceType.Science, 5).map(_.head)

      } yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map { (firstAtomIds, secondAtomIds) =>
      assertEquals(firstAtomIds.distinct.length, 1)
      assertEquals(secondAtomIds.distinct.length, 1)
    }

  }
}