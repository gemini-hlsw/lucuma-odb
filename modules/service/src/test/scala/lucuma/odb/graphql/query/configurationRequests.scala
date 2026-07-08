// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.UpdateObservationsOps

class configurationRequests 
  extends OdbSuite 
     with ObservingModeSetupOperations 
     with UpdateObservationsOps {

  val pi1   = TestUsers.Standard.pi(1, 30)
  val pi2   = TestUsers.Standard.pi(2, 31)
  val admin = TestUsers.Standard.admin(3, 32)
  val validUsers = List(pi1, pi2, admin)

  private def expectRequests(user: User, ids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          configurationRequests(
            WHERE: {
              id: {
                IN: ${ids.asJson}
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """,
      expected = Right(json"""                
        {
          "configurationRequests" : {
            "matches": ${
              ids.sorted.map { id => json"""{ "id": $id }""" }
            }
          }
        }                
      """)
    )

  // set up cfp, program, and fully configured observation
  private def setupAs(user: User): IO[ConfigurationRequest.Id] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(user, "Foo")
      _     <- addProposal(user, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(user, pid)
      oid   <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      rid   <- createConfigurationRequestAs(user, oid)
    yield rid

  test("create and select some configuration requests as different users"):
    for
      mine   <- setupAs(pi1).replicateA(5)
      theirs <- setupAs(pi2).replicateA(5)
      _      <- expectRequests(pi1, mine)
      _      <- expectRequests(pi2, theirs)
    yield ()

  private def expectMatches(user: User, where: String, ids: List[ConfigurationRequest.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          configurationRequests(
            WHERE: $where
          ) {
            matches {
              id
            }
          }
        }
      """,
      expected = Right(json"""
        {
          "configurationRequests" : {
            "matches": ${
              ids.sorted.map { id => json"""{ "id": $id }""" }
            }
          }
        }
      """)
    )

  // set up cfp, program, and fully configured observation, with a justification
  private def setupWithJustificationAs(user: User, justification: String): IO[ConfigurationRequest.Id] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(user, "Foo")
      _     <- addProposal(user, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(user, pid)
      oid   <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      rid   <- createConfigurationRequestAs(user, oid, NonEmptyString.from(justification).toOption)
    yield rid

  test("filter by justification"):
    for
      rid <- setupWithJustificationAs(pi1, "It is dark out there.")
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, justification: { LIKE: "%dark%" } }""", List(rid))
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, justification: { LIKE: "%bright%" } }""", Nil)
    yield ()

  test("filter by feedback"):
    for
      rid <- setupWithJustificationAs(pi1, "Some justification.")
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, feedback: { IS_NULL: true } }""", List(rid))
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, feedback: { IS_NULL: false } }""", Nil)
      _   <- query(
               user = admin,
               query = s"""
                 mutation {
                   updateConfigurationRequests(input: {
                     SET: { feedback: "Too ambitious." }
                     WHERE: { id: { EQ: ${rid.asJson} } }
                   }) {
                     requests { id }
                   }
                 }
               """
             )
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, feedback: { LIKE: "%ambitious%" } }""", List(rid))
    yield ()

  test("filter by createdAt and updatedAt"):
    for
      rid <- setupAs(pi1)
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, createdAt: { GT: "1970-01-01 00:00:00" } }""", List(rid))
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, createdAt: { LT: "1970-01-01 00:00:00" } }""", Nil)
      _   <- expectMatches(pi1, s"""{ id: { EQ: ${rid.asJson} }, updatedAt: { GT: "1970-01-01 00:00:00" } }""", List(rid))
    yield ()

}
