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
import lucuma.core.model.Client
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.Timestamp

class addSlewEvent extends OdbSuite:

  val pi: User      = TestUsers.Standard.pi(nextId, nextId)
  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(pi, service)

  private def createObservation(
    mode: ObservingModeType,
    user: User
  ):IO[Observation.Id] =
    for
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
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
      service,
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

  test("addSlewEvent - unknown visit"):
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
      service,
      _ => query,
      _ => s"Observation 'o-42' not found.".asLeft
    )

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
        _  <- addSlewEventAs(service, o, SlewStage.StartSlew)
        v  <- recordVisitAs(service, Instrument.GmosNorth, o)
        _  <- addSlewEventAs(service, o, SlewStage.EndSlew)
        _  <- addSequenceEventAs(service, v, SequenceCommand.Start)
        vs <- visits(o)
      yield checkVisits(vs)

  test("record visit then slew"):
    assertIOBoolean:
      for
        o  <- createObservation(ObservingModeType.GmosNorthLongSlit, pi)
        v  <- recordVisitAs(service, Instrument.GmosNorth, o)
        _  <- addSlewEventAs(service, o, SlewStage.StartSlew)
        _  <- addSlewEventAs(service, o, SlewStage.EndSlew)
        _  <- addSequenceEventAs(service, v, SequenceCommand.Start)
        vs <- visits(o)
      yield checkVisits(vs)

  test("no static"):
    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: o =>
      addSlewEventAs(service, o, SlewStage.StartSlew) *>
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

  def addClientId(
    oid:         Observation.Id,
    cid:         Client.Id,
    isDuplicate: Boolean
  ): IO[Unit] =
      expect(
        service,
        s"""
          mutation {
            addSlewEvent(input: {
              observationId: "$oid",
              slewStage: START_SLEW,
              clientId: "$cid"
            }) {
              event { clientId }
            }
          }
        """,
        Either.cond(
          !isDuplicate,
          json"""
            {
              "addSlewEvent": {
                "event": {
                  "clientId": $cid
                }
              }
            }
          """,
          List(s"An event with client id '$cid' has already been added.")
        )
      )

  test("addSlewEvent - client id"):
    val cid  = Client.Id.parse("c-530c979f-de98-472f-9c23-a3442f2a9f7f")

    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: oid =>
      addClientId(oid, cid.get, isDuplicate = false)

  test("addSlewEvent - duplicate client id"):
    val cid  = Client.Id.parse("c-b7044cd8-38b5-4592-8d99-91d2c512041d")

    createObservation(ObservingModeType.GmosNorthLongSlit, pi).flatMap: oid =>
      addClientId(oid, cid.get, isDuplicate = false) *>
      addClientId(oid, cid.get, isDuplicate = true)