// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime

class executionTimestamps extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  case class Times(
    first:      Option[Timestamp],
    start:      Option[Timestamp],
    last:       Option[Timestamp],
    completion: Option[Timestamp],
    events:     List[Timestamp],
    nonSlew:    List[Timestamp]
  )

  def times(oid: Observation.Id): IO[Times] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              timestamps {
                firstEventTime
                startTime
                lastEventTime
                completionTime
              }
              events {
                matches {
                  effectiveTime
                  eventType
                }
              }
            }
          }
        }
      """
    ).flatMap: js =>
      val e = js.hcursor.downFields("observation", "execution")
      val t = e.downField("timestamps")
      (for
        f  <- t.downField("firstEventTime").as[Option[Timestamp]]
        s  <- t.downField("startTime").as[Option[Timestamp]]
        l  <- t.downField("lastEventTime").as[Option[Timestamp]]
        c  <- t.downField("completionTime").as[Option[Timestamp]]
        es <- e.downFields("events", "matches").values.toList.flatten.traverse: ev =>
                (ev.hcursor.downField("effectiveTime").as[Timestamp], ev.hcursor.downField("eventType").as[String]).tupled
      yield Times(f, s, l, c, es.map(_._1), es.collect { case (t, tpe) if tpe =!= "SLEW" => t })).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  def newObservation: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  def completeObservation(oid: Observation.Id): IO[Unit] =
    for
      ss <- scienceStepIds(serviceUser, oid)
      v  <- recordVisitAs(serviceUser, oid)
      _  <- ss.traverse(sid => addEndStepEvent(sid, v))
    yield ()

  def startObservation(oid: Observation.Id): IO[Unit] =
    for
      v  <- recordVisitAs(serviceUser, oid)
      ss <- firstScienceAtomStepIds(serviceUser, oid)
      _  <- ss.take(3).traverse(sid => addEndStepEvent(sid, v))
    yield ()

  // (start, end) of each visit's events, in visit order.
  def visitIntervals(oid: Observation.Id): IO[List[(Timestamp, Timestamp)]] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              visits {
                matches {
                  interval {
                    start
                    end
                  }
                }
              }
            }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("observation", "execution", "visits", "matches")
        .values.toList.flatten
        .traverse: v =>
          val i = v.hcursor.downField("interval")
          (i.downField("start").as[Timestamp], i.downField("end").as[Timestamp]).tupled
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  // First and last event times always span the recorded events.
  def assertEventRange(t: Times): Unit =
    assertEquals(t.first, t.events.minimumOption, "firstEventTime")
    assertEquals(t.start, t.nonSlew.minimumOption, "startTime")
    assertEquals(t.last,  t.events.maximumOption, "lastEventTime")

  test("no events: everything is null"):
    for
      oid <- newObservation
      t   <- times(oid)
    yield assertEquals(t, Times(None, None, None, None, Nil, Nil))

  test("slew only: event times, but not complete"):
    for
      oid <- newObservation
      _   <- addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)
      t   <- times(oid)
    yield
      assertEquals(t.events.size, 1)
      assertEventRange(t)
      assertEquals(t.start, None)
      assertEquals(t.completion, None)

  test("start time is the first event other than a slew"):
    for
      oid <- newObservation
      _   <- addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)
      _   <- startObservation(oid)
      t   <- times(oid)
    yield
      assertEventRange(t)
      assert(t.start.isDefined)
      assertNotEquals(t.start, t.first)

  test("ongoing: event times, but not complete"):
    for
      oid <- newObservation
      _   <- startObservation(oid)
      t   <- times(oid)
    yield
      assert(t.events.nonEmpty)
      assertEventRange(t)
      assertEquals(t.completion, None)

  test("completed: completion time is the last event time"):
    for
      oid <- newObservation
      _   <- completeObservation(oid)
      t   <- times(oid)
    yield
      assertEventRange(t)
      assert(t.last.isDefined)
      assertEquals(t.completion, t.last)

  test("multiple visits: first event in the first visit, last in the latest"):
    for
      oid <- newObservation
      ss  <- scienceStepIds(serviceUser, oid)
      v0  <- recordVisitAs(serviceUser, oid)
      _   <- ss.take(3).traverse(sid => addEndStepEvent(sid, v0))
      v1  <- recordVisitAs(serviceUser, oid)
      _   <- ss.drop(3).traverse(sid => addEndStepEvent(sid, v1))
      vs  <- visitIntervals(oid)
      t   <- times(oid)
    yield
      assertEquals(vs.size, 2)
      val List((s0, e0), (s1, e1)) = vs: @unchecked
      assert(!s1.toInstant.isBefore(e0.toInstant), "visits do not overlap")
      assertEventRange(t)
      assertEquals(t.first, s0.some)
      assertEquals(t.last, e1.some)
      assertEquals(t.completion, t.last)

  test("events after completion move the completion time"):
    for
      oid <- newObservation
      _   <- completeObservation(oid)
      t0  <- times(oid)
      v   <- recordVisitAs(serviceUser, oid)
      _   <- addSequenceEventAs(serviceUser, v, SequenceCommand.Start)
      t1  <- times(oid)
    yield
      assertEventRange(t1)
      assertEquals(t1.events.size, t0.events.size + 1)
      assertEquals(t1.completion, t1.last)
      assert(
        (t0.completion, t1.completion).tupled.exists((c0, c1) => c0.toInstant.isBefore(c1.toInstant)),
        s"completion time moved: $t0 -> $t1"
      )

  test("declared complete without events: no completion time"):
    for
      oid <- newObservation
      _   <- declareCompleteDirectly(oid)
      t   <- times(oid)
    yield assertEquals(t, Times(None, None, None, None, Nil, Nil))

  test("declared complete with events: completion time is the last event time"):
    for
      oid <- newObservation
      _   <- startObservation(oid)
      _   <- declareCompleteDirectly(oid)
      t   <- times(oid)
    yield
      assertEventRange(t)
      assert(t.last.isDefined)
      assertEquals(t.completion, t.last)

  test("declared ongoing after completion: no completion time"):
    for
      oid <- newObservation
      _   <- completeObservation(oid)
      t0  <- times(oid)
      _   <- declareOngoingDirectly(oid)
      t1  <- times(oid)
    yield
      assert(t0.completion.isDefined)
      assertEventRange(t1)
      assert(t1.last.isDefined)
      assertEquals(t1.completion, None)

  test("many observations in one query"):
    for
      p  <- createProgram
      tg <- createTargetWithProfileAs(pi, p)
      o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(tg))
      o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(tg))
      o2 <- createGmosNorthLongSlitObservationAs(pi, p, List(tg))
      _  <- startObservation(o1)
      _  <- completeObservation(o2)
      t1 <- times(o1)
      t2 <- times(o2)
      js <- query(
              user  = pi,
              query = s"""
                query {
                  program(programId: "$p") {
                    allGroupElements {
                      observation {
                        id
                        execution {
                          timestamps {
                            completionTime
                          }
                        }
                      }
                    }
                  }
                }
              """
            )
    yield
      val completions: Map[Observation.Id, Json] =
        js.hcursor
          .downFields("program", "allGroupElements")
          .values.toList.flatten
          .flatMap: e =>
            val o = e.hcursor.downField("observation")
            o.downField("id").as[Observation.Id].toOption.map: id =>
              id -> o.downFields("execution", "timestamps", "completionTime").focus.getOrElse(Json.Null)
          .toMap
      assertEquals(t1.completion, None)
      assert(t2.completion.isDefined)
      assertEquals(
        completions,
        Map(o0 -> Json.Null, o1 -> Json.Null, o2 -> t2.completion.get.asJson)
      )
