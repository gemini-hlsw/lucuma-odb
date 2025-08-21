// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.ServiceUser
import lucuma.core.syntax.timespan.*

import scala.concurrent.duration.*

// N.B. this works locally, most of the time. Need to get it working reliably.
// @IgnoreSuite
class programEdit extends OdbSuite with SubscriptionUtils {

  override val httpRequestHandler = invitationEmailRequestHandler

  object Group1 {
    val pi       = TestUsers.Standard.pi(11, 110)
    val guest    = TestUsers.guest(12)
    val service  = TestUsers.service(13)
  }

  object Group2 {
    val pi       = TestUsers.Standard.pi(21, 210)
    val guest    = TestUsers.guest(22)
    val service  = TestUsers.service(23)
  }

  def validUsers =
    List(
      Group1.pi, Group1.guest, Group1.service,
      Group2.pi, Group2.guest, Group2.service,
    )

  test("trigger for my own new programs") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      val allEvents = List(
        json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
        json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo" } } }""",
        json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
        json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "bar" } } }"""
      )
      val expected = user match {
        case ServiceUser(_, _) => List(allEvents(0), allEvents(2)) // service users are not linked
        case _                 => allEvents // pi and guest generate a separate update when linked
      }

      subscriptionExpect(
        user = user,
        query =
          """
            subscription {
              programEdit {
                editType
                value {
                  name
                }
              }
            }
          """,
        mutations =
          Right(
            createProgram(user, "foo") >>
            createProgram(user, "bar")
          ),
        expected = expected
      )
    }
  }

  test("trigger for my own new programs (but nobody else's) as guest user") {
    import Group2._
    subscriptionExpect(
      user = guest,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo" } } }"""
        )
    )
  }

  test("trigger for my own new programs (but nobody else's) as standard user in PI role") {
    import Group2._
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "bar" } } }"""
        )
    )
  }

  test("trigger for all programs as service user") {
    import Group2._
    subscriptionExpect(
      user = service,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo") >>
          createProgram(pi, "bar") >>
          createProgram(service, "baz")
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "bar" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "bar" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "baz" } } }"""
        )
    )
  }

  test("edit event should show up") {
    import Group2._
    subscriptionExpect(
      user = service,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(guest, "foo").flatMap { id =>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            query(
              service,
              s"""
              mutation {
                updatePrograms(input: {
                  WHERE: { id: { EQ: "$id" } }
                  SET: { name: "foo2" }
                }) {
                  programs {
                    id
                  }
                }
              }
              """
            )
          } >>
          createProgram(service, "baz").void
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo" } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo2" } } }""",
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "baz" } } }""",
        )
    )
  }

  test("work even if no database fields are selected") {
    import Group1.pi
    subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          programEdit {
            editType
          }
        }
      """,
      mutations =
        Right(
          createProgramAs(pi).replicateA(2)
        ),
      expected = List(
        json"""{"programEdit":{"editType":"CREATED"}}""",
        json"""{"programEdit":{"editType":"UPDATED"}}""",
        json"""{"programEdit":{"editType":"CREATED"}}""",
        json"""{"programEdit":{"editType":"UPDATED"}}"""
      )
    )
  }

  test("edit event should show up for proposal creation and update") {
    import Group2.pi
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                proposal {
                  category
                }
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(pi, "foo").flatMap { pid =>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            addProposal(pi, pid) >>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            query(
              pi,
              s"""
              mutation {
                updateProposal(input: {
                  programId: "$pid"
                  SET: { category: COSMOLOGY }
                }) {
                  proposal {
                    category
                  }
                }
              }
              """
            )
          }
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "proposal": null } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": null } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": { "category": "GALACTIC_OTHER" } } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": { "category": "COSMOLOGY" } } } }"""
        )
    )
  }

  test("edit event should show up for updating the partner splits") {
    import Group2.pi
    import Group2.service
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                proposal {
                  type {
                    ... on Queue {
                      partnerSplits {
                        partner
                        percent
                      }
                    }
                  }
                }
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(pi, "foo").flatMap { pid =>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            createCallForProposalsAs(service).flatMap(addQueueProposal(pi, pid, _)) >>
            IO.sleep(1.second) >> // give time to see the creation before we do an update
            query(
              pi,
              s"""
              mutation {
                updateProposal(input: {
                  programId: "$pid"
                  SET: {
                    type: {
                      queue: {
                        partnerSplits: [
                          {
                            partner: US
                            percent: 60
                          },
                          {
                            partner: AR
                            percent: 40
                          }
                        ]
                      }
                    }
                  }
                }) {
                  proposal {
                    category
                  }
                }
              }
              """
            )
          }
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "proposal": null } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": null } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": { "type": { "partnerSplits": [] } } } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "proposal": { "type": { "partnerSplits": [ { "partner" : "US", "percent" : 60 }, { "partner" : "AR", "percent" : 40 }] } } } } }"""
        )
    )
  }

  test("edit event should show up for linking a user") {
    import Group2.pi
    import Group1.pi as pi2
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                users {
                  role
                }
              }
            }
          }
        """,
      mutations =
        Right(
          createProgram(pi, "foo").flatMap: pid =>
            IO.sleep(1.second) >>
            addProgramUserAs(pi, pid).flatMap: mid =>
              IO.sleep(1.second) >> // give time to see the creation before we do an update
              linkUserAs(pi, mid, pi2.id)
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "users": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "users": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "users": [ { "role": "COI" } ] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "users": [ { "role": "COI" } ] } } }"""
        )
    )
  }

  test("edit event should show up for creating and modifying invitations") {
    import Group2.pi
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                userInvitations {
                  status
                  recipientEmail
                }
              }
            }
          }
        """,
      mutations =
        Right(
          for {
            pid <- createProgram(pi, "foo")
            _   <- IO.sleep(1.second)
            mid <- addProgramUserAs(pi, pid)
            _   <- IO.sleep(1.second)
            inv <- createUserInvitationAs(pi, mid, recipientEmail = EmailAddress.From.getOption("here@there.com").get)
            _   <- IO.sleep(1.second)
            _   <- revokeUserInvitationAs(pi, inv.id)
          } yield ()
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [ { "status": "PENDING", "recipientEmail": "here@there.com" } ] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [ { "status": "REVOKED", "recipientEmail": "here@there.com" } ] } } }"""
        )
    )
  }

  test("edit event should show up for updating an invitation's email status") {
    import Group2.pi
    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                userInvitations {
                  status
                  email {
                    status
                  }
                }
              }
            }
          }
        """,
      mutations =
        Right(
          for {
            pid <- createProgram(pi, "foo")
            _   <- IO.sleep(1.second)
            mid <- addProgramUserAs(pi, pid)
            inv <- createUserInvitationAs(pi, mid, recipientEmail = EmailAddress.From.getOption("here@there.com").get)
            _   <- IO.sleep(1.second)
            eid <- getEmailIdForInvitation(inv.id)
            _   <- updateEmailStatus(eid.get, EmailStatus.Accepted)
          } yield ()
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [ { "status": "PENDING", "email": { "status": "QUEUED"}} ] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "userInvitations": [ { "status": "PENDING", "email": { "status": "ACCEPTED"}} ] } } }"""
        )
    )
  }

  test("edit event should show up for setting allocations") {
    import Group2.pi
    import Group2.service
    import lucuma.odb.graphql.input.AllocationInput

    val allocs = List(
      AllocationInput(TimeAccountingCategory.AR, ScienceBand.Band1, 1.hrTimeSpan),
      AllocationInput(TimeAccountingCategory.BR, ScienceBand.Band2, 30.minTimeSpan)
    )

    val allocsJson =
    json"""
      [
        {
          "category": "AR",
          "scienceBand": "BAND1",
          "duration": {
            "minutes": 60.000000
          }
        },
        {
          "category": "BR",
          "scienceBand": "BAND2",
          "duration": {
            "minutes": 30.000000
          }
        }
      ]
    """

    subscriptionExpect(
      user = pi,
      query =
        """
          subscription {
            programEdit {
              editType
              value {
                name
                allocations {
                  category
                  scienceBand
                  duration {
                    minutes
                  }
                }
              }
            }
          }
        """,
      mutations =
        Right(
          for {
            p <- createProgram(pi, "foo")
            _ <- IO.sleep(1.second)
            _ <- setAllocationsAs(service, p, allocs)
            _ <- IO.sleep(1.second)
          } yield ()
        ),
      expected =
        List(
          json"""{ "programEdit": { "editType" : "CREATED", "value": { "name": "foo", "allocations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "allocations": [] } } }""",
          json"""{ "programEdit": { "editType" : "UPDATED", "value": { "name": "foo", "allocations": $allocsJson } } }""",
        )
    )
  }

}
