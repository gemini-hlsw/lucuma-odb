// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.json.all.transport.given

class executionSciFlamingos2 extends ExecutionTestSupportForFlamingos2:

  test("simple generation - limited future"):
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
                 ${flamingos2ScienceQuery(1.some)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "flamingos2" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> flamingos2ExpectedScienceAtom((0, 15), (0, -15)),
                      "possibleFuture" -> List.empty[Json].asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )

  test("arc, flat, one science"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Arc, Flamingos2ArcStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Flat, Flamingos2FlatStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Science, StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
      yield o

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${flamingos2ScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "flamingos2" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> Json.obj(
                        "description" -> s"Long Slit 1px, JH, R1200JH".asJson,
                        "observeClass" -> "SCIENCE".asJson,
                        "steps" -> List(
                          flamingos2ExpectedScience(0, -15),
                          flamingos2ExpectedScience(0, -15),
                          flamingos2ExpectedScience(0,  15)
                        ).asJson
                      ),
                      "possibleFuture" -> List.empty[Json].asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )

  test("execute a step then fail it"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))

        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Science, StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _ <- addEndStepEvent(s)

        d  <- recordDatasetAs(serviceUser, s, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

      yield o

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${flamingos2ScienceQuery(1.some)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "flamingos2" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> flamingos2ExpectedScienceAtom((0, 15), (0, -15)),
                      "possibleFuture" -> List.empty[Json].asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )