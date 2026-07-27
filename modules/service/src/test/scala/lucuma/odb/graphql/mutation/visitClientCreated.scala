// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.util.Timestamp

import java.time.Instant

class visitClientCreated extends OdbSuite with query.ExecutionTestSupportForGmos:

  private def createObservation(user: User): IO[Observation.Id] =
    for
      pid <- createProgramAs(user)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, ObservingModeType.GmosNorthLongSlit.some, tid)
    yield oid

  private def nowMinus(seconds: Long): Timestamp =
    Timestamp.unsafeFromInstantTruncated(Instant.now().minusSeconds(seconds))

  private def timeArg(time: Option[Timestamp]): String =
    time.fold("")(t => s"""time: "${t.isoFormat}"""")

  // (recordedTime, effectiveTime)
  private def recordVisitTimes(oid: Observation.Id, time: Option[Timestamp]): IO[(Timestamp, Timestamp)] =
    query(
      serviceUser,
      s"""
        mutation {
          recordVisit(input: {
            observationId: "$oid"
            ${timeArg(time)}
          }) {
            visit {
              recordedTime
              effectiveTime
            }
          }
        }
      """
    ).map: json =>
      val c = json.hcursor.downFields("recordVisit", "visit")
      (c.downField("recordedTime").require[Timestamp], c.downField("effectiveTime").require[Timestamp])

  // (recordedTime, effectiveTime) of the observation's single visit
  private def visitTimes(oid: Observation.Id): IO[(Timestamp, Timestamp)] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              visits {
                matches {
                  recordedTime
                  effectiveTime
                }
              }
            }
          }
        }
      """
    ).map: json =>
      val c = json.hcursor.downFields("observation", "execution", "visits", "matches").downN(0)
      (c.downField("recordedTime").require[Timestamp], c.downField("effectiveTime").require[Timestamp])

  test("recordVisit - supplied time drives effectiveTime"):
    val t = nowMinus(60)
    createObservation(pi).flatMap: oid =>
      recordVisitTimes(oid, t.some).map: (created, effective) =>
        assertEquals(effective, t)
        assertNotEquals(created, t)

  test("recordVisit - omitted time defaults to created"):
    createObservation(pi).flatMap: oid =>
      recordVisitTimes(oid, None).map: (created, effective) =>
        assertEquals(effective, created)

  private val outOfRange: String =
    "The supplied visit creation time is outside the acceptable range."

  test("recordVisit - future time is rejected"):
    val t = Timestamp.unsafeFromInstantTruncated(Instant.now().plusSeconds(3600))
    createObservation(pi).flatMap: oid =>
      expect(
        serviceUser,
        s"""
          mutation {
            recordVisit(input: {
              observationId: "$oid"
              ${timeArg(t.some)}
            }) { visit { id } }
          }
        """,
        List(outOfRange).asLeft
      )

  private def addSlewWithTime(oid: Observation.Id, stage: SlewStage, time: Option[Timestamp]): String =
    s"""
      mutation {
        addSlewEvent(input: {
          observationId: "$oid"
          slewStage: ${stage.tag.toUpperCase}
          ${timeArg(time)}
        }) {
          event { id }
        }
      }
    """

  test("addSlewEvent - creating a visit uses the supplied time as effectiveTime"):
    val t = nowMinus(60)
    for
      oid <- createObservation(pi)
      _   <- query(serviceUser, addSlewWithTime(oid, SlewStage.StartSlew, t.some))
      ts  <- visitTimes(oid)
      (created, effective) = ts
    yield
      assertEquals(effective, t)
      assertNotEquals(created, t)

  test("addSlewEvent - a visit-creating slew with an out-of-range time is rejected"):
    val t = Timestamp.unsafeFromInstantTruncated(Instant.now().minusSeconds(7200))
    createObservation(pi).flatMap: oid =>
      expect(serviceUser, addSlewWithTime(oid, SlewStage.StartSlew, t.some), List(outOfRange).asLeft)
