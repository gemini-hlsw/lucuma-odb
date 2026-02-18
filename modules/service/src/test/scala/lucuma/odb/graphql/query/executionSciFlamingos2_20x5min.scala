// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.Clock
import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.step_id
import skunk.*
import skunk.implicits.*

// Match an example in the F2 Long Slit requirements spreadsheet
class executionSciFlamingos2_20x5min extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 5.minuteTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(20))

  val abba = flamingos2ExpectedScienceAtom(ExposureTime, (0, 15, Enabled), (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled))

  test("simple generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15)),
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  // Adjust the timestamp of step records precisely
  def adjustStepTime(s: Step.Id, start: Timestamp, end: Timestamp): IO[Unit] =
    val query: Command[(Step.Id, Timestamp, Timestamp)] =
      sql"""
        UPDATE t_step
           SET c_first_event_time = $core_timestamp,
               c_last_event_time  = $core_timestamp
         WHERE c_step_id = $step_id
      """.command.contramap { (s, t0, t1) => (t0, t1, s) }

    withSession: session =>
      session.execute(query)(s, start, end).void

  private def recordAbba(a: Atom.Id, v: Visit.Id, rt: Timestamp): IO[Unit] =
    for
      a0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
      _  <- addEndStepEvent(a0, v)
      _  <- adjustStepTime(a0, rt,                   rt +|  5.minTimeSpan)
      b0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
      _  <- addEndStepEvent(b0, v)
      _  <- adjustStepTime(b0, rt +|  5.minTimeSpan, rt +| 10.minTimeSpan)
      b1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
      _  <- addEndStepEvent(b1, v)
      _  <- adjustStepTime(b1, rt +| 10.minTimeSpan, rt +| 15.minTimeSpan)
      a1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
      _  <- addEndStepEvent(a1, v)
      _  <- adjustStepTime(a1, rt +| 15.minTimeSpan, rt +| 20.minTimeSpan)
    yield ()

  private def recordCals(a: Atom.Id, v: Visit.Id, rt: Timestamp): IO[Unit] =
    for
      f <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Flat, Flamingos2FlatStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
      _ <- addEndStepEvent(f, v)
      _ <- adjustStepTime(f, rt, rt +| 15.secTimeSpan)
      r <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2Arc, Flamingos2ArcStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
      _ <- addEndStepEvent(r, v)
      _ <- adjustStepTime(r, rt +| 15.secTimeSpan, rt +| (15 + 32).secTimeSpan)
    yield ()

  test("after ABBA 1"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    abba,
                    flamingos2ExpectedGcals((0, 15)),
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("after ABBA 2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    flamingos2ExpectedGcals((0, 15)),
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("after ABBA 3"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
        _  <- recordAbba(a, v, rt +| 40.minTimeSpan)
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
                  "possibleFuture" -> List(
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("after mid-science cals"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
        _  <- recordAbba(a, v, rt +| 40.minTimeSpan)
        _  <- recordCals(a, v, rt +| 60.minTimeSpan)
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("after ABBA 4"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
        _  <- recordAbba(a, v, rt +| 40.minTimeSpan)
        _  <- recordCals(a, v, rt +| 60.minTimeSpan)
        _  <- recordAbba(a, v, rt +| (60 + 15 + 32).minTimeSpan)
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
                  "nextAtom" -> abba,
                  "possibleFuture" -> List(
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("after ABBA 5"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
        _  <- recordAbba(a, v, rt +| 40.minTimeSpan)
        _  <- recordCals(a, v, rt +| 60.minTimeSpan)
        _  <- recordAbba(a, v, rt +| (60 + 15 + 32).minTimeSpan)
        _  <- recordAbba(a, v, rt +| (80 + 15 + 32).minTimeSpan)
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

  test("after end-science cals"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        rt <- Clock[IO].realTimeInstant.map(Timestamp.unsafeFromInstantTruncated)
        _  <- recordAbba(a, v, rt)
        _  <- recordAbba(a, v, rt +| 20.minTimeSpan)
        _  <- recordAbba(a, v, rt +| 40.minTimeSpan)
        _  <- recordCals(a, v, rt +| 60.minTimeSpan)
        _  <- recordAbba(a, v, rt +| ( 60 + 15 + 32).minTimeSpan)
        _  <- recordAbba(a, v, rt +| ( 80 + 15 + 32).minTimeSpan)
        _  <- recordCals(a, v, rt +| (100 + 15 + 32).minTimeSpan)
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

  test("stop after first step"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _ <- addEndStepEvent(s, v)
        _ <- addSequenceEventAs(serviceUser, v, SequenceCommand.Stop)
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
                  "possibleFuture" -> List(
                    // Gcals to finish up the "stopped" block
                    flamingos2ExpectedGcals((0, 15)),

                    // New block.  In theory the operator or scheduler will
                    // stop after the gcals.  At any rate, only 80 minutes
                    // remaining so no mid-period calibration
                    abba,
                    abba,
                    abba,
                    abba,
                    flamingos2ExpectedGcals((0, 15))
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )
