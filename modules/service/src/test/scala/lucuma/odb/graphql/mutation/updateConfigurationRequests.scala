// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class updateConfigurationRequests extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(3, 34)
  val admin   = TestUsers.Standard.admin(2, 32)

  val validUsers = List(pi, pi2, admin).toList

  object updateConfigurationRequestAs {

    def query(rid: ConfigurationRequest.Id, status: ConfigurationRequestStatus, justification: Option[NonEmptyString] = None, feedback: Option[NonEmptyString] = None): String =
      s"""
        mutation {
          updateConfigurationRequests(input: {
            SET: {
              status: ${status.tag.toUpperCase}
              justification: ${justification.asJson}
              ${feedback.foldMap(f => s"feedback: ${f.asJson}")}
            }
            WHERE: { id: { EQ: ${rid.asJson} } }
          }) {
            requests {
              id
              status
              justification
              feedback
            }
            hasMore
          }
        }
      """

    def apply(user: User, rid: ConfigurationRequest.Id, status: ConfigurationRequestStatus, justification: Option[NonEmptyString] = None, feedback: Option[NonEmptyString] = None): IO[Unit] =
      expect(
        user = user,
        query = query(rid, status, justification, feedback),
        expected = Right(json"""
          {
            "updateConfigurationRequests" : {
              "requests" : [
                {
                  "id" : $rid,
                  "status" : $status,
                  "justification": $justification,
                  "feedback": $feedback
                }
              ],
              "hasMore" : false
            }
          }
        """)
      )

  }

  val setup: IO[ConfigurationRequest.Id] =
    for     
      cid <- createGeminiCallForProposalsAs(admin)
      pid <- createProgramAs(pi, "Foo")
      _   <- addProposal(pi, pid, Some(cid), None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      mid <- createConfigurationRequestAs(pi, oid)
    yield mid

  ConfigurationRequestStatus.values.foreach: status =>
    test(s"Admin should be able to set status to $status."):
      setup.flatMap(updateConfigurationRequestAs(admin, _, status))

  List(ConfigurationRequestStatus.Requested, ConfigurationRequestStatus.Withdrawn).foreach: status =>
    test(s"PI should be able to set status to $status."):
      setup.flatMap(updateConfigurationRequestAs(pi, _, status))

  List(ConfigurationRequestStatus.Approved, ConfigurationRequestStatus.Denied).foreach: status =>
    test(s"PI should *not* be able to set status to $status."):
      interceptOdbError(setup.flatMap(updateConfigurationRequestAs(pi, _, status))):
        case OdbError.NotAuthorized(_, _) => () // expected

  test(s"PI can't set status on another user's request (update affects no rows)"):
    setup.flatMap: rid =>
      expect(
        user = pi2,
        query = updateConfigurationRequestAs.query(rid, ConfigurationRequestStatus.Withdrawn),
        expected = Right(json"""
          {
            "updateConfigurationRequests" : {
              "requests" : [],
              "hasMore" : false
            }
          }
        """)
      )

  test("Should be able to update justification."):
    setup.flatMap: rid =>
      updateConfigurationRequestAs(pi, rid, ConfigurationRequestStatus.Requested, NonEmptyString.from("A new justification.").toOption)

  val someFeedback = NonEmptyString.from("Please narrow the conditions.").toOption

  test("Admin should be able to set feedback."):
    setup.flatMap(updateConfigurationRequestAs(admin, _, ConfigurationRequestStatus.Denied, feedback = someFeedback))

  test("Admin should be able to set feedback without changing status."):
    setup.flatMap(updateConfigurationRequestAs(admin, _, ConfigurationRequestStatus.Requested, feedback = someFeedback))

  test("PI should *not* be able to set feedback."):
    interceptOdbError(setup.flatMap(updateConfigurationRequestAs(pi, _, ConfigurationRequestStatus.Requested, feedback = someFeedback))):
      case OdbError.NotAuthorized(_, _) => () // expected

  test("PI should *not* be able to clear feedback."):
    interceptOdbError(
      setup.flatMap: rid =>
        query(
          user = pi,
          query = s"""
            mutation {
              updateConfigurationRequests(input: {
                SET: { feedback: null }
                WHERE: { id: { EQ: ${rid.asJson} } }
              }) {
                requests { id }
              }
            }
          """
        )
    ):
      case OdbError.NotAuthorized(_, _) => () // expected

  test("updatedAt should advance on update, createdAt should not."):
    def timestamps(rid: ConfigurationRequest.Id): IO[(Timestamp, Timestamp)] =
      query(
        user = admin,
        query = s"""
          query {
            configurationRequests(WHERE: { id: { EQ: ${rid.asJson} } }) {
              matches {
                createdAt
                updatedAt
              }
            }
          }
        """
      ).map: json =>
        val c = json.hcursor.downFields("configurationRequests", "matches").downArray
        (c.downField("createdAt").require[Timestamp], c.downField("updatedAt").require[Timestamp])
    for
      rid          <- setup
      (cre1, upd1) <- timestamps(rid)
      _            <- updateConfigurationRequestAs(admin, rid, ConfigurationRequestStatus.Approved, feedback = someFeedback)
      (cre2, upd2) <- timestamps(rid)
    yield
      assertEquals(cre1, cre2)
      assertEquals(cre1, upd1)
      assert(upd2 > upd1)

}
