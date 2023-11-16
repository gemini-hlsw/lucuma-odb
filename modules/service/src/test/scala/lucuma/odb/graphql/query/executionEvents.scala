// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.ObservingModeType

class executionEvents extends OdbSuite with ExecutionQuerySetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  test("observation -> execution -> events") {
    recordAll(pi, mode, offset = 0, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events() {
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
    recordAll(pi, mode, offset = 100).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events() {
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

  private def extractEvents(
    vid: Visit.Id
  ): IO[List[ExecutionEvent]] =
    withServices(service) { services =>
      services.session.transaction.use { xa =>
        services
          .executionEventService
          .streamEvents(vid)(using xa)
          .compile
          .toList
      }
    }

  test("simple events stream") {
    for {
      on <- recordAll(service, mode, offset=200, visitCount = 3)
      vs = on.visits
      es <- vs.traverse(v => extractEvents(v.id))
    } yield
      vs.zip(es).foreach { case (visit, events) =>
        assertEquals(events.size, visit.allEvents.size)
        assert(events.forall(_.visitId === visit.id))
      }
  }

  // `streamEvents` for a vist should include all events from the first to the
  // last including events for other visits that are interleaved.  This is to
  // accommodate "overlap event" (non) charging.  When you start a second visit
  // before the first has finished, the first visit is no longer charged.  In
  // other words, no double charging.  To detect this, we need all the events
  // that were generated chronologically from the start to the end of the events
  // for a visit.

  test("interleaved event stream") {
    for {
      on0 <- recordAll(service, mode, offset=300)
      on1 <- recordAll(service, mode, offset=400)
      vs0 = on0.visits.last
      vs1 = on1.visits.last
      se  <- addSequenceEventAs(service, vs0.id, SequenceCommand.Continue)
      es  <- extractEvents(vs0.id)
    } yield
      assertEquals(
        es.map(_.id),
        (on0.allEvents ::: on1.allEvents ::: List(se)).map(_.id)
      )
  }

}
