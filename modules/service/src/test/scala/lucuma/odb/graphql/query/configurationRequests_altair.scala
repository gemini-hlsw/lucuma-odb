// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.UpdateObservationsOps

/**
 * A configuration request is approved for exactly one Altair mode: `Configuration.subsumes`
 * requires the two modes to be equal, so an approval obtained on NGS says nothing about the same
 * observation once it switches to LGS, or drops adaptive optics altogether.
 */
class configurationRequests_altair
  extends OdbSuite
     with ObservingModeSetupOperations
     with UpdateObservationsOps:

  val pi: User    = TestUsers.Standard.pi(1, 30)
  val admin: User = TestUsers.Standard.admin(2, 31)

  val validUsers: List[User] = List(pi, admin)

  // Altair is Gemini North AO, so only a GNIRS observation can carry it.
  private def setup: IO[Observation.Id] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Altair")
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(pi, pid)
      oid   <- createGnirsLongSlitObservationAs(pi, pid, tid)
    yield oid

  private def setAltair(oid: Observation.Id, mode: Option[String]): IO[Unit] =
    updateObservation(
      pi,
      oid,
      s"targetEnvironment: { altair: ${mode.fold("null")(m => s"{ mode: $m }")} }",
      "observations { id }",
      json"""{ "updateObservations": { "observations": [ { "id": $oid } ] } }""".asRight
    )

  private def approvedRequestFor(oid: Observation.Id): IO[ConfigurationRequest.Id] =
    for
      rid <- createConfigurationRequestAs(pi, oid)
      _   <- setConfigurationRequestStatusAs(admin, rid, ConfigurationRequestStatus.Approved)
    yield rid

  private def expectRequests(oid: Observation.Id, rids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            configurationRequests {
              id
              status
            }
          }
        }
      """,
      expected = json"""
        {
          "observation": {
            "configurationRequests": ${rids.map(rid => json"""{ "id": $rid, "status": "APPROVED" }""")}
          }
        }
      """.asRight
    )

  private def assertApprovalDoesNotCarryOver(approved: String, switchedTo: Option[String]): IO[Unit] =
    for
      oid <- setup
      _   <- setAltair(oid, approved.some)
      rid <- approvedRequestFor(oid)
      _   <- expectRequests(oid, List(rid))
      _   <- setAltair(oid, switchedTo)
      _   <- expectRequests(oid, Nil)
    yield ()

  test("an NGS approval does not apply once the observation switches to LGS"):
    assertApprovalDoesNotCarryOver("NGS", "LGS".some)

  test("an LGS approval does not apply once the observation switches to NGS"):
    assertApprovalDoesNotCarryOver("LGS", "NGS".some)

  test("an NGS approval does not apply once Altair is removed"):
    assertApprovalDoesNotCarryOver("NGS", none)

  test("an approval without Altair does not apply once Altair is added"):
    for
      oid <- setup
      rid <- approvedRequestFor(oid)
      _   <- expectRequests(oid, List(rid))
      _   <- setAltair(oid, "NGS".some)
      _   <- expectRequests(oid, Nil)
    yield ()

  private def expectAltairMode(oid: Observation.Id, rid: ConfigurationRequest.Id, mode: Option[String]): IO[Unit] =
    expect(
      user  = pi,
      query = s"""
        query {
          observation(observationId: "$oid") {
            configuration {
              altairMode
            }
          }
          configurationRequests(WHERE: { id: { EQ: ${rid.asJson} } }) {
            matches {
              configuration {
                altairMode
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "observation": {
            "configuration": {
              "altairMode": $mode
            }
          },
          "configurationRequests": {
            "matches": [
              {
                "configuration": {
                  "altairMode": $mode
                }
              }
            ]
          }
        }
      """.asRight
    )

  test("the Altair mode is exposed on the observation and on the request"):
    for
      oid <- setup
      _   <- setAltair(oid, "LGS_P1".some)
      rid <- createConfigurationRequestAs(pi, oid)
      _   <- expectAltairMode(oid, rid, "LGS_P1".some)
    yield ()

  test("without Altair the mode is null on the observation and on the request"):
    for
      oid <- setup
      rid <- createConfigurationRequestAs(pi, oid)
      _   <- expectAltairMode(oid, rid, none)
    yield ()
