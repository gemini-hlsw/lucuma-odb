// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.core.model.User

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

  test("slew then record visit"):
    val vid = for
      o  <- createObservation(ObservingModeType.GmosNorthLongSlit, pi)
      s0 <- addSlewEventAs(service, o, SlewStage.StartSlew)
      v  <- recordVisitAs(service, Instrument.GmosNorth, o)
      s1 <- addSlewEventAs(service, o, SlewStage.EndSlew)
      e  <- addSequenceEventAs(service, v, SequenceCommand.Start)
    yield Set(v, s0._4, s1._4, e._4)

    assertIOBoolean(vid.map(_.sizeIs == 1))

  test("record visit then slew"):
    val vid = for
      o  <- createObservation(ObservingModeType.GmosNorthLongSlit, pi)
      v  <- recordVisitAs(service, Instrument.GmosNorth, o)
      s0 <- addSlewEventAs(service, o, SlewStage.StartSlew)
      s1 <- addSlewEventAs(service, o, SlewStage.EndSlew)
      e  <- addSequenceEventAs(service, v, SequenceCommand.Start)
    yield Set(v, s0._4, s1._4, e._4)

    assertIOBoolean(vid.map(_.sizeIs == 1))