// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.InvitationStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

class program extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)
  val staff    = TestUsers.Standard.staff(4, 104)

  val validUsers = List(pi, guest, service, staff)

  override val httpRequestHandler = invitationEmailRequestHandler

  test("any user can read their own programs") {
    List(guest, pi, service).traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgramAs(user, name).flatMap { id =>
        expect(
          user = user,
          query =
            s"""
              query {
                program(programId: "$id") {
                  id
                  name
                  userInvitations {
                    status
                  }
                  proposal {
                    title
                    category
                    abstract
                  }
                }
              }
            """,
          expected = Right(
            json"""
              {
                "program": {
                  "id": $id,
                  "name": $name,
                  "userInvitations": [],
                  "proposal": null
                }
              }
            """
          )
        )
      }
    }
  }

  test("invitations are visible"):
    List(pi, service).traverse: user =>
      val name = s"${user.displayName}'s Science Program"
      createProgramAs(user, name).flatMap: pid =>
        addProgramUserAs(user, pid).flatMap: mid =>
          createUserInvitationAs(user, mid) >>
          expect(
            user  = user,
            query =
              s"""
                query {
                  program(programId: "$pid") {
                    id
                    name
                    userInvitations {
                      status
                      issuer { id }
                      programUser {
                        id
                        partnerLink {
                          linkType
                          ... on HasPartner {
                            partner
                          }
                        }
                      }
                      email {
                        status
                      }
                    }
                  }
                }
              """,
            expected =
              json"""
                {
                  "program": {
                    "id": $pid,
                    "name": $name,
                    "userInvitations": [
                      {
                        "status": ${InvitationStatus.Pending},
                        "issuer": { "id": ${user.id} },
                        "programUser": {
                          "id": $mid,
                          "partnerLink": {
                            "linkType": "HAS_PARTNER",
                            "partner": "US"
                          }
                        },
                        "email": {
                          "status": ${EmailStatus.Queued}
                        }
                      }
                    ]
                  }
                }
              """.asRight
          )

  test("guest and standard user can't see each others' programs") {
    val users = List(guest, pi)
    users.traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgramAs(user, name).flatMap { id =>
        users.traverse { user2 =>
          expect(
            user = user2,
            query =
              s"""
                query {
                  program(programId: "$id") {
                    id
                    name
                  }
                }
              """,
            expected =
              if (user == user2) {
                Right(
                  json"""
                    {
                      "program": {
                        "id": $id,
                        "name": $name
                      }
                    }
                  """
                )
              } else {
                Right(
                  json"""
                    {
                      "program": null
                    }
                  """
                )
              }
          )
        }
      }
    }
  }

  test("service user can see anyone's programs") {
    val users = List(guest, pi)
    users.traverse { user =>
      val name = s"${user.displayName}'s Science Program"
      createProgramAs(user, name).flatMap { id =>
        expect(
          user = service,
          query =
            s"""
              query {
                program(programId: "$id") {
                  id
                  name
                }
              }
            """,
          expected =
            Right(
              json"""
                {
                  "program": {
                    "id": $id,
                    "name": $name
                  }
                }
              """
            )
        )
      }
    }
  }

  test("program / observations (simple)") {
    createProgramAs(pi, "program with some observations").flatMap { pid =>
      createObservationAs(pi, pid).replicateA(3).flatMap { oids =>
        expect(
          user = pi,
          query =
             s"""
              query {
                program(programId: "$pid") {
                  id
                  observations() {
                    matches {
                      id
                    }
                  }
                }
              }
            """,
          expected =
            Right(
              json"""
                {
                  "program": {
                    "id": $pid,
                    "observations" : {
                      "matches": ${oids.map{id => Json.obj("id" -> id.asJson)}}
                    }
                  }
                }
              """
            )
        )
      }
    }
  }

  test("program / observations (with limit)") {
    createProgramAs(pi, "program with some observations").flatMap { pid =>
      createObservationAs(pi, pid).replicateA(3).flatMap { oids =>
        expect(
          user = pi,
          query =
             s"""
              query {
                program(programId: "$pid") {
                  id
                  observations(LIMIT: 2) {
                    matches {
                      id
                    }
                  }
                }
              }
            """,
          expected =
            Right(
              json"""
                {
                  "program": {
                    "id": $pid,
                    "observations" : {
                      "matches": ${oids.take(2).map{id => Json.obj("id" -> id.asJson)}}
                    }
                  }
                }
              """
            )
        )
      }
    }
  }

  test("program / timeCharge placeholder") {
    createProgramAs(pi, "program with no executed time").flatMap { pid =>
      expect(
        user = pi,
        query =
          s"""
            query {
              program(programId: "$pid") {
                timeCharge {
                  time {
                    program { seconds }
                    partner { seconds }
                    nonCharged { seconds }
                    total { seconds }
                  }
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "program": {
                  "timeCharge": []
                }
              }
            """
          )

      )
    }
  }

  test("program without calibration role") {
    createProgramAs(pi, "program without calibration role").flatMap { pid =>
      expect(
        user = staff,
        query =
          s"""
            query {
              program(programId: "$pid") {
                calibrationRole
              }
            }
          """,
          expected = Right(
            json"""
              {
                "program": {
                  "calibrationRole": null
                }
              }
            """
          )

      )
    }
  }

  test("program with calibration role") {
    expect(
      user = staff,
      query =
        s"""
          query {
            programs(WHERE: {type: {EQ: SYSTEM}}) {
                matches {
                  calibrationRole
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "programs": {
                  "matches": [
                    {
                      "calibrationRole": "SPECTROPHOTOMETRIC"
                    },
                    {
                      "calibrationRole": "TWILIGHT"
                    }
                  ]
                }
              }
            """
          )

      )
    }
  }
