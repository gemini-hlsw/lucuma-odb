// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package mutation

import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.StandardRole
import lucuma.sso.service.database.RoleRequest

class addRole extends GraphQLSuite with SsoSuite with Fixture with FlakyTests:

  List(RoleRequest.Staff, RoleRequest.Ngo(Partner.AR)).foreach: role =>
    test(s"$role should not be able to give Bob a role"):
      flaky():
        AsBob.queryIds
          .flatMap: (bob, _) =>
            AsAlice
              .queryIds
              .flatMap: 
                case (alice, _) =>
                  AsAlice.withRoleRequest(role)
                    .expectQuery(
                      query = 
                        s"""
                          mutation {
                            addRole(
                              userId: "$bob"
                              roleType: STAFF
                            )
                          }
                        """,
                      expected =
                        json"""
                          {
                            "errors" : [
                              {
                                "message" : ${s"User $alice is not authorized to perform this action."}
                              }
                            ]                  
                          }
                        """
                    )

  test("Admin Alice should be able to give Bob a role"):
    flaky():
      As(Bob)
        .queryIds
        .flatMap: bob =>
          AsAlice.withRoleRequest(RoleRequest.Admin)
            .query:
              s"""
                mutation {
                  addRole(
                    userId: "${bob._1}"
                    roleType: STAFF
                  )
                }
              """
            .map: json =>
              assert:
                json
                  .hcursor
                  .downFields("data", "addRole")
                  .as[StandardRole.Id]
                  .isRight

  test("Admin Alice should be able to give Bob an NGO role"):
    flaky():
      As(Bob)
        .queryIds
        .flatMap: bob =>
          AsAlice.withRoleRequest(RoleRequest.Admin)
            .query:
              s"""
                mutation {
                  addRole(
                    userId: "${bob._1}"
                    roleType: NGO
                    partner: US
                  )
                }
              """
            .map: json =>
              assert:
                json
                  .hcursor
                  .downFields("data", "addRole")
                  .as[StandardRole.Id]
                  .isRight

  test("Admin Alice should be able to give Bob an Admin role, and aftewards he should be able to give a Staff role to Alice"):
    flaky():
      AsBob
        .queryIds
        .flatMap: (bob, _) =>
          As(Alice)
            .queryIds
            .flatMap: (alice, _) =>
              AsAlice.withRoleRequest(RoleRequest.Admin)
                .query:
                  s"""
                    mutation {
                      addRole(
                        userId: "$bob"
                        roleType: ADMIN
                      )
                    }
                  """
                .flatMap: json =>
                  val newRole = json.hcursor.downFields("data", "addRole").require[StandardRole.Id]
                  AsBob.withRole(newRole)
                    .query:
                      s"""
                        mutation {
                          addRole(
                            userId: "$alice"
                            roleType: STAFF
                          )
                        }
                      """
                    .map: json =>
                      assert:
                        json
                          .hcursor
                          .downFields("data", "addRole")
                          .as[StandardRole.Id]
                          .isRight  

