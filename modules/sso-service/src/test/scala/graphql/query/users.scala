// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.orcid.OrcidIdGenerator

class users extends GraphQLSuite with SsoSuite with Fixture with OrcidIdGenerator[IO]:

  // Do this to ensure that Alice and Bob have sequential userids
  lazy val setup: IO[Unit] =
    List(AsAlice, AsBob).traverse(_.canonicalizeUser).void

  test("Standard user can only see self (and can see full record)."):
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

  test("Staff can see many users (filter for orcid id)."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query =
        s"""
          query {
            users(
              WHERE: {
                orcidId: {
                  EQ: "${BobOrcidId.value}"
                }
              }
            ) {
              matches {
                id
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
                    "id" : "u-103",
                    "orcidId" : ${BobOrcidId}
                  }
                ]
              }
            }
          }
        """
    )

  test("Staff can see many users (filter for orcid id in uri form)."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query =
        s"""
          query {
            users(
              WHERE: {
                orcidId: {
                  EQ: "${BobOrcidId.uri}"
                }
              }
            ) {
              matches {
                id
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
                    "id" : "u-103",
                    "orcidId" : ${BobOrcidId}
                  }
                ]
              }
            }
          }
        """
    )

  test("Staff can see many users (filter for orcid id in a list)."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query =
        s"""
          query {
            users(
              WHERE: {
                orcidId: {
                  IN: ["${AliceOrcidId.value}", "${BobOrcidId.value}"]
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """,
        expected = json"""
          {
            "data" : {
              "users" : {
                "matches" : [
                  { "id" : "u-101" },
                  { "id" : "u-103" }
                ]
              }
            }
          }
        """
    )

  test("Staff can see many users (filter for missing orcid id)."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query =
        """
          query {
            users(
              WHERE: {
                orcidId: {
                  IS_NULL: true
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """,
        expected = json"""
          {
            "data" : {
              "users" : {
                "matches" : [
                  { "id" : "u-100" }
                ]
              }
            }
          }
        """
    )

  test("Staff can see many users (filter for orcid id with LIKE)."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query =
        s"""
          query {
            users(
              WHERE: {
                orcidId: {
                  LIKE: "%${BobOrcidId.value.takeRight(4)}"
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """,
        expected = json"""
          {
            "data" : {
              "users" : {
                "matches" : [
                  { "id" : "u-103" }
                ]
              }
            }
          }
        """
    )

  test("An invalid orcid id filter is rejected."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).query(
      """
        query {
          users(
            WHERE: {
              orcidId: {
                EQ: "not-an-orcid-id"
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """
    ).map: json =>
      assert(json.hcursor.downField("errors").focus.exists(_.spaces2.contains("Invalid ORCID id")), json.spaces2)

  test("Andy issue ('exceeded maximum input value depth')."):
    setup >>
    AsBob.withRoleRequest(RoleRequest.Staff).expectQuery(
      query = 
        """
          query {
            users(
              WHERE: {
                AND: [
                  { 
                    profile: {
                      email: {
                        LIKE: "%noirlab%", 
                        MATCH_CASE: false
                      }
                    }
                  }, 
                  {
                    OR: [
                      {
                        profile: {
                          givenName: {
                            LIKE: "%andrew%", 
                            MATCH_CASE: false
                          }
                        }
                      }, 
                      {
                        profile: {
                          familyName: {
                            LIKE: "%andrew%", 
                            MATCH_CASE: false
                          }
                        }
                      }
                    ]
                  }
                ]
              }
            ) {
              matches {
                id
                profile {
                  givenName
                  familyName
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
                "matches" : []
              }
            }
          }
        """
    )
