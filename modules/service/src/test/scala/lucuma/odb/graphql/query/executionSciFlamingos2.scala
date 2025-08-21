// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given

class executionSciFlamingos2 extends ExecutionTestSupportForFlamingos2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("simple generation - limited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, 15, Enabled), (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                  "possibleFuture" -> List(flamingos2ExpectedGcals((0, 15))).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one science"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _ <- addEndStepEvent(s)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                  "possibleFuture" -> List(flamingos2ExpectedGcals((0, 15))).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one cycle"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedGcals((0, 15)),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("one sequence"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
        s4 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Arc, Flamingos2ArcStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s4)
        s5 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Flat, Flamingos2FlatStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s5)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.Null
              )
            )
          ).asRight
      )

  test("repeat failed step right away"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
        s4 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s4)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedGcals((0, 15)),
                  "possibleFuture" -> List.empty[Json].asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("failed step later"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1001.fits")
        _  <- setQaState(d, DatasetQaState.Usable)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "flamingos2" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                  "possibleFuture" -> List(flamingos2ExpectedGcals((0, 15))).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("not on slit"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <-
          createObservationWithModeAs(
            pi,
            p,
            List(t),
            s"""
              flamingos2LongSlit: {
                disperser: R1200_JH
                filter: JH
                fpu: LONG_SLIT_1
                explicitOffsets: [
                  {
                    p: { arcseconds:  60 }
                    q: { arcseconds:   0 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds: 100 }
                  },
                  {
                    p: { arcseconds:   0 }
                    q: { arcseconds: 100 }
                  },
                  {
                    p: { arcseconds: 60 }
                    q: { arcseconds:  0 }
                  }
                ]
              }
            """
          )
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2ScienceQuery(oid),
        expected =
          List(
            s"Could not generate a sequence for $oid: At least one exposure must be taken on slit."
          ).asLeft
      )
