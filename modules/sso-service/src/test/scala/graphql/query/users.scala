// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import cats.effect.IO
import io.circe.literal.*
import lucuma.core.model.StandardUser
import lucuma.sso.service.database.RoleRequest

class users extends GraphQLSuite with SsoSuite with Fixture with FlakyTests:

  test("Standard user can only see self (and can see full record)."):
    flaky():
      var bob: StandardUser = null // sorry
      As(Bob).expectQueryWithUser(
        query = b =>
          bob = b
          """
            query {
              users() {
                matches {
                  id
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
                  "id" : ${bob.id},
                  "orcidId" : ${bob.profile.orcidId},
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
      As(Bob, withRole = Some(RoleRequest.Staff)).query(
        """
          query {
            users() {
              matches {
                id
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
        """
      ).flatMap: j =>
        IO.println(j.spaces2)


