// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

/**
 * Configuration requests carry the scheduling mode as part of the configuration:
 * an approved mode covers itself and every looser one.  The Target of Opportunity
 * activation plays no part; it is approved program-wide, as the proposal's
 * ceiling.  The UNINTERRUPTIBLE a ToO activation forces is matched like any other
 * mode.
 */
class observation_configurationRequests_scheduling extends OdbSuite with ObservingModeSetupOperations:

  val pi: User    = TestUsers.Standard.pi(1, 30)
  val admin: User = TestUsers.Standard.admin(2, 31)

  val validUsers: List[User] = List(pi, admin)

  private def setup: IO[(Program.Id, Observation.Id)] =
    for
      cid <- createGeminiCallForProposalsAs(admin)
      pid <- createProgramAs(pi, "Scheduling approval")
      _   <- addProposal(pi, pid, cid.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
    yield (pid, oid)

  private def update(oid: Observation.Id, set: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { $set }
            WHERE: { id: { EQ: "$oid" } }
          }) { observations { id } }
        }
      """
    ).void

  private def setScheduling(oid: Observation.Id, constraints: String): IO[Unit] =
    update(oid, s"schedulingConstraints: { $constraints }")

  private def approve(rid: ConfigurationRequest.Id): IO[Unit] =
    setConfigurationRequestStatusAs(admin, rid, ConfigurationRequestStatus.Approved)

  private def requestsFor(oid: Observation.Id): IO[List[ConfigurationRequest.Id]] =
    query(pi, s"""query { observation(observationId: "$oid") { configurationRequests { id } } }""")
      .map:
        _.hcursor
         .downField("observation")
         .downField("configurationRequests")
         .require[List[Json]]
         .map(_.hcursor.downField("id").require[ConfigurationRequest.Id])

  private def applicableObservations(rid: ConfigurationRequest.Id): IO[List[Observation.Id]] =
    query(pi, s"""query { configurationRequests(WHERE: { id: { EQ: "$rid" } }) { matches { applicableObservations } } }""")
      .map:
        _.hcursor
         .downField("configurationRequests")
         .downField("matches")
         .require[List[Json]]
         .head
         .hcursor
         .downField("applicableObservations")
         .require[List[Observation.Id]]

  test("a request records the observation's scheduling mode"):
    for
      (_, oid) <- setup
      _        <- setScheduling(oid, "tooActivation: RAPID")
      _        <- createConfigurationRequestAs(pi, oid)
      _        <- expect(
                    pi,
                    s"""
                      query {
                        observation(observationId: "$oid") {
                          configurationRequests { configuration { schedulingMode } }
                        }
                      }
                    """,
                    json"""
                      {
                        "observation": {
                          "configurationRequests": [
                            { "configuration": { "schedulingMode": "UNINTERRUPTIBLE" } }
                          ]
                        }
                      }
                    """.asRight
                  )
    yield ()

  test("a request for a looser mode does not cover a stricter one"):
    for
      (_, oid) <- setup
      _        <- createConfigurationRequestAs(pi, oid)
      strict   <- cloneObservationAs(pi, oid)
      _        <- setScheduling(strict, "schedulingMode: UNINTERRUPTIBLE")
      rs       <- requestsFor(strict)
    yield assertEquals(rs, Nil)

  test("a request for a stricter mode covers a looser one"):
    for
      (_, oid) <- setup
      _        <- setScheduling(oid, "schedulingMode: UNINTERRUPTIBLE")
      rid      <- createConfigurationRequestAs(pi, oid)
      looser   <- cloneObservationAs(pi, oid)
      _        <- setScheduling(looser, "schedulingMode: NO_SPLITTING")
      rs       <- requestsFor(looser)
    yield assertEquals(rs, List(rid))

  // A ToO runs UNINTERRUPTIBLE, so it needs a configuration approved at that mode;
  // an approval for ordinary science is not enough.
  test("a ToO is not covered by a configuration approved at a looser mode"):
    for
      (_, oid) <- setup
      plain    <- createConfigurationRequestAs(pi, oid)
      _        <- approve(plain)
      too      <- cloneObservationAs(pi, oid)
      _        <- setScheduling(too, "tooActivation: RAPID")
      rs       <- requestsFor(too)
      obs      <- applicableObservations(plain)
    yield
      assertEquals(rs, Nil)
      assertEquals(obs, List(oid))

  // The activation is no part of the match: an approval at UNINTERRUPTIBLE covers
  // a ToO even when it was made for ordinary science.
  test("a ToO is covered by a configuration approved at UNINTERRUPTIBLE, whatever its activation"):
    for
      (_, oid) <- setup
      _        <- setScheduling(oid, "schedulingMode: UNINTERRUPTIBLE")
      rid      <- createConfigurationRequestAs(pi, oid)
      _        <- approve(rid)
      too      <- cloneObservationAs(pi, oid)
      _        <- setScheduling(too, "tooActivation: INTERRUPTING")
      rs       <- requestsFor(too)
      obs      <- applicableObservations(rid)
    yield
      assertEquals(rs, List(rid))
      assertEquals(obs.toSet, Set(oid, too))
