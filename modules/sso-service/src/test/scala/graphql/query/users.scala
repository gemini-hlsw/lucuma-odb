// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.orcid.OrcidIdGenerator

class users extends GraphQLSuite with SsoSuite with Fixture with FlakyTests with OrcidIdGenerator[IO]:

  // Do this to ensure that Alice and Bob have sequential userids
  lazy val setup: IO[Unit] =
    List(AsAlice, AsBob).traverse(_.canonicalizeUser).void

  test("Standard user can only see self (and can see full record)."):
    flaky():
      setup >>
      AsBob.expectQuery(
        query =
          """
            query {
              users() {
                matches {
                  orcidId
                  type
                  enabled
                  roles {
                    type              
                  }
                  profile {
                    givenName
                    familyName
                    creditName
                    email
                  }
                }
              }
            }
          """,  
        expected = json"""
        {
          "data" : {
            "users" : {
              "matches" : [
                {
                  "orcidId" : $BobOrcidId,
                  "type" : "STANDARD",
                  "enabled" : true,
                  "roles" : [
                    {
                      "type" : "PI"
                    }
                  ],
                  "profile" : {
                    "givenName" : "Bob",
                    "familyName" : "Dobbs",
                    "creditName" : null,
                    "email" : "bob@dobbs.com"
                  }
                }
              ]
            }
          }
        }
        """
      )

  test("Staff can see many users."):
    flaky():
      setup >>
      AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
        query = 
          """
            query {
              users() {
                matches {
                  id
                  type
                  orcidId
                }
              }
            }
          """,
          expected = json"""
            {
              "data" : {
                "users" : {
                  "matches" : [
                    {
                      "id" : "u-100",
                      "type" : "SERVICE",
                      "orcidId" : null
                    },
                    {
                      "id" : "u-101",
                      "type" : "STANDARD",
                      "orcidId" : ${AliceOrcidId}
                    },
                    {
                      "id" : "u-103",
                      "type" : "STANDARD",
                      "orcidId" : ${BobOrcidId}
                    }
                  ]
                }
              }
            }
          """
      )

  test("Staff can see many users (filter for type)."):
    flaky():
      setup >>
      AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
        query = 
          """
            query {
              users(
                WHERE: {
                  type: {
                    EQ: STANDARD
                  }
                }
              ) {
                matches {
                  id
                  type
                  orcidId
                }
              }
            }
          """,
          expected = json"""
            {
              "data" : {
                "users" : {
                  "matches" : [
                    {
                      "id" : "u-101",
                      "type" : "STANDARD",
                      "orcidId" : ${AliceOrcidId}
                    },
                    {
                      "id" : "u-103",
                      "type" : "STANDARD",
                      "orcidId" : ${BobOrcidId}
                    }
                  ]
                }
              }
            }
          """
      )

