// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.Site
import lucuma.core.enums.SlewStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp

class addSlewEvent extends OdbSuite with query.ExecutionTestSupportForGmos:

  private def createObservation(
    mode: ObservingModeType,
    user: User
  ):IO[Observation.Id] =
    for
      pid <- createProgramAs(user)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, mode.some, tid)
    yield oid

  private def addSlewEventTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Observation.Id => String,
    expected: Observation.Id => Either[String, Json]
  ): IO[Unit] =
    for
      oid <- createObservation(mode, user)
      _   <- expect(user, query(oid), expected(oid).leftMap(s => List(s)))
    yield ()

  test("addSlewEvent"):
    def query(oid: Observation.Id): String =
      s"""
        mutation {
          addSlewEvent(input: {
            observationId: "$oid",
            slewStage: START_SLEW
          }) {
            event {
              eventType
              observation {
                id
              }
              ... on SlewEvent {
                slewStage
              }
            }
          }
        }
      """

    addSlewEventTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      oid => query(oid),
      oid => json"""
      {
        "addSlewEvent": {
          "event": {
            "eventType": "SLEW",
            "observation": {
              "id": $oid
            },
            "slewStage": "START_SLEW"
          }
        }
      }
      """.asRight
    )

  test("addSlewEvent - unknown observation"):
    def query: String =
      s"""
        mutation {
          addSlewEvent(input: {
            observationId: "o-42",
            slewStage: START_SLEW
          }) {
            event {
              observation {
                id
              }
            }
          }
        }
      """

    addSlewEventTest(
      ObservingModeType.GmosNorthLongSlit,
      serviceUser,
      _ => query,
      _ => s"Observation 'o-42' not found or is not associated with any instrument.".asLeft
    )

  test("addSlewEvent - no instrument"):
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- interceptGraphQL(s"Observation '$oid' not found or is not associated with any instrument."):
               addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)
    yield ()

  def visits(o: Observation.Id): IO[List[(Visit.Id, ObservingNight)]] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$o") {
            execution {
              visits {
                matches {
                  id
                  site
                  created
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("observation", "execution", "visits", "matches")
        .values
        .toList
        .flatten
        .traverse: vjson =>
          val c = vjson.hcursor
          for
            v <- c.downField("id").as[Visit.Id]
            s <- c.downField("site").as[Site]
            t <- c.downField("created").as[Timestamp]
          yield (v, ObservingNight.fromSiteAndInstant(s, t.toInstant))
        .fold(e => throw new RuntimeException(e.message), identity)

  def checkVisits(visits: List[(Visit.Id, ObservingNight)]): Boolean =
    visits match
      case v :: Nil                  => true            // one visit expected
      case (_, n0) :: (_, n1) :: Nil => n0.next === n1  // could be two if straddling an observing night
      case _                         => false           // otherwise something is wrong


  test("slew then record visit"):
    assertIOBoolean:
      for
        o  <- createObservation(ObservingModeType.GmosNorthLongSlit, pi)
        _  <- addSlewEventAs(serviceUser, o, SlewStage.StartSlew)
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        _  <- addSlewEventAs(serviceUser, o, SlewStage.EndSlew)
        _  <- addSequenceEventAs(serviceUser, v, SequenceCommand.Start)
        vs <- visits(o)
      yield checkVisits(vs)

  test("record visit then slew"):
    assertIOBoolean:
      for
        o  <- createObservation(ObservingModeType.GmosNorthLongSlit, pi)
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        _  <- addSlewEventAs(serviceUser, o, SlewStage.StartSlew)
        _  <- addSlewEventAs(serviceUser, o, SlewStage.EndSlew)
        _  <- addSequenceEventAs(serviceUser, v, SequenceCommand.Start)
        vs <- visits(o)
      yield checkVisits(vs)

  test("no static"):
    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: o =>
      addSlewEventAs(serviceUser, o, SlewStage.StartSlew) *>
      expect(
        user     = pi,
        query    = s"""
          query {
            observation(observationId: "$o") {
              execution {
                visits {
                  matches {
                    gmosNorth {
                      stageMode
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "execution": {
                "visits": {
                  "matches": [
                    {
                      "gmosNorth": null
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )

  def addWithIdempotencyKey(
    oid: Observation.Id,
    idm: Option[IdempotencyKey] = None
  ): IO[(ExecutionEvent.Id, Option[IdempotencyKey])] =
    query(
      serviceUser,
      s"""
        mutation {
          addSlewEvent(input: {
            observationId: "$oid",
            slewStage: START_SLEW,
            ${idm.fold("")(idm => s"idempotencyKey: \"$idm\"")}
          }) {
            event {
              id
              idempotencyKey
            }
          }
        }
      """).flatMap: js =>
        val cur = js.hcursor.downFields("addSlewEvent", "event")
        (for
          e <- cur.downField("id").as[ExecutionEvent.Id]
          n <- cur.downField("idempotencyKey").as[Option[IdempotencyKey]]
        yield (e, n)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  test("addSlewEvent - idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b9bac66c-4e12-4b1d-b646-47c2c3a97792")

    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: oid =>
      assertIO(addWithIdempotencyKey(oid, idm = idm).map(_._2), idm)

  test("addSlewEvent - duplicate idempotency key"):
    val idm = IdempotencyKey.FromString.getOption("b7044cd8-38b5-4592-8d99-91d2c512041d")

    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: oid =>
      addWithIdempotencyKey(oid, idm = idm).flatMap: (eid, _) =>
        assertIO(addWithIdempotencyKey(oid, idm = idm).map(_._1), eid)