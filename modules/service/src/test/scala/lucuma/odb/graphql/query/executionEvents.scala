// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.DatasetEvent
import lucuma.core.model.ExecutionEvent.SequenceEvent
import lucuma.core.model.ExecutionEvent.SlewEvent
import lucuma.core.model.ExecutionEvent.StepEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.util.Timestamp
import lucuma.odb.data.ObservingModeType

class executionEvents extends OdbSuite with ExecutionQuerySetupOperations {

  import ExecutionQuerySetupOperations.Setup
  import ExecutionQuerySetupOperations.VisitNode

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  override def recordVisit(
    mode:    ObservingModeType,
    setup:   Setup,
    user:    User,
    oid:     Observation.Id,
    visit:   Int
  ): IO[VisitNode] =
    for {
      vid <- recordVisitAs(user, mode.instrument, oid)
      e0  <- addSlewEventAs(user, vid, SlewStage.StartSlew)
      e1  <- addSlewEventAs(user, vid, SlewStage.EndSlew)
      e2  <- addSequenceEventAs(user, vid, SequenceCommand.Start)
      as  <- (0 until setup.atomCount).toList.traverse { a => recordAtom(mode, setup, user, vid, visit, a) }
      e3  <- addSequenceEventAs(user, vid, SequenceCommand.Stop)
    } yield VisitNode(vid, as, List(e0, e1, e2, e3))


  test("observation -> execution -> events") {
    recordAll(pi, service, mode, offset = 0, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val events = on.allEvents.map(e => Json.obj("id" -> e.id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "events": {
              "matches": $events
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("observation -> execution -> events (visit, observation)") {
    recordAll(pi, service, mode, offset = 100).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events {
                matches {
                  id
                  observation {
                    id
                  }
                  visit {
                    id
                  }
                }
              }
            }
          }
        }
      """

      val events = on.allEvents.map { e =>
        Json.obj(
          "id" -> e.id.asJson,
          "observation" -> Json.obj("id" -> on.id.asJson),
          "visit" -> Json.obj("id" -> on.visits.head.id.asJson)
        )
      }

      val e = json"""
      {
        "observation": {
          "execution": {
            "events": {
              "matches": $events
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (no WHERE, only visible to pi2)") {
    recordAll(pi2, service, mode, offset = 200, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events {
            matches {
              id
            }
          }
        }
      """

      val events = on.allEvents.map(e => Json.obj("id" -> e.id.asJson))

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi2, q, e)
    }
  }

  test("query -> events (WHERE observationId)") {
    recordAll(pi, service, mode, offset = 300, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events = on.allEvents.map(e => Json.obj("id" -> e.id.asJson))

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + eventType SEQUENCE)") {
    recordAll(pi, service, mode, offset = 400, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              eventType: {
                EQ: SEQUENCE
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case SequenceEvent(id, _, _, _, _) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + eventType SLEW)") {
    recordAll(pi, service, mode, offset = 450, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              eventType: {
                EQ: SLEW
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case SlewEvent(id, _, _, _, _) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + received)") {
    recordAll(pi, service, mode, offset = 500).flatMap { on =>
      val start: Timestamp = on.allEvents.head.received
      val end: Timestamp   = on.allEvents.last.received

      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              received: {
                GT: "$start",
                LT: "$end"
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.tail.init.map(e => Json.obj("id" -> e.id.asJson))

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE visitId)") {
    recordAll(pi, service, mode, offset = 600, visitCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              visitId: {
                EQ: "${on.visits.head.id}"
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.visits.head.allEvents.map(e => Json.obj("id" -> e.id.asJson))

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE id)") {
    recordAll(pi, service, mode, offset = 700).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              id: {
                EQ: "${on.allEvents.head.id}"
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         List(Json.obj("id" -> on.allEvents.head.id.asJson))

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + sequenceEvent command)") {
    recordAll(pi, service, mode, offset = 800, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              sequenceCommand: {
                EQ: START
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case SequenceEvent(id, _, _, _, SequenceCommand.Start) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + stepEvent stepId)") {
    recordAll(pi, service, mode, offset = 900, stepCount = 2).flatMap { on =>
      val sid = on.visits.head.atoms.head.steps.head.id

      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              eventType: {
                EQ: STEP
              },
              stepId: {
                EQ: "$sid"
              }
            }
          ) {
            matches {
              id
              ... on StepEvent {
                step {
                  id
                }
              }
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case StepEvent(id, _, _, _, _, `sid`, _) =>
           Json.obj(
             "id" -> id.asJson,
             "step" -> Json.obj(
               "id" -> sid.asJson
             )
           )
         }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + stepEvent stepStage)") {
    recordAll(pi, service, mode, offset = 1000, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              stepStage: {
                EQ: END_STEP
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case StepEvent(id, _, _, _, _, _, StepStage.EndStep) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE datasetId)") {
    recordAll(pi, service, mode, offset = 1100, stepCount = 2).flatMap { on =>
      val dids = on.visits.head.atoms.head.steps.head.allDatasets
      val q = s"""
        query {
          events(
            WHERE: {
              datasetId: {
                IN: ${dids.map(id => s"\"${id.toString}\"").mkString("[", ", ", "]")}
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val didsContains = dids.toSet
      val events: List[Json] =
         on.allEvents.collect { case DatasetEvent(id, _, _, _, _, _, did, _) if didsContains(did) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + datasetStage)") {
    recordAll(pi, service, mode, offset = 1200, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              datasetStage: {
                EQ: END_WRITE
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case DatasetEvent(id, _, _, _, _, _, _, DatasetStage.EndWrite) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("query -> events (WHERE observationId + slewEvent slewStage)") {
    recordAll(pi, service, mode, offset = 1300, stepCount = 2).flatMap { on =>
      val q = s"""
        query {
          events(
            WHERE: {
              observationId: {
                EQ: "${on.id}"
              },
              slewStage: {
                EQ: END_SLEW
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """

      val events: List[Json] =
         on.allEvents.collect { case SlewEvent(id, _, _, _, SlewStage.EndSlew) => Json.obj("id" -> id.asJson) }

      val e = json"""
      {
        "events": {
          "matches": $events
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

}
