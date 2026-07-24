// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package mutation

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.StandardUser
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.database.RoleType
import lucuma.sso.service.orcid.OrcidIdGenerator

class deleteRole extends GraphQLSuite with SsoSuite with Fixture with FlakyTests with OrcidIdGenerator[IO]:

  extension (as: As)      
    def queryRoleTypes: IO[Set[RoleType]] =
      as.query("query { user { roles { type } } }").map: json =>
        json.hcursor.downFields("data", "user", "roles").require[Set[Json]].map: j =>
          j.hcursor.downField("type").require[RoleType]

  List(None, Some(RoleRequest.Staff), Some(RoleRequest.Ngo(Partner.CA))).foreach: rr =>
    test(s"${rr.getOrElse(RoleRequest.Pi).tpe} can't call deleteRole"):
      flaky():
        var bob: StandardUser = null // i'm sorry
        rr.foldLeft(As(Bob))(_.withRoleRequest(_))
          .expectQueryWithUser(
            u => { bob = u; s"""mutation { deleteRole(roleId: "${bob.role.id}") }""" },
            json"""
              {
                "errors" : [
                  {
                    "message" : ${s"User ${bob.id} is not authorized to perform this action."}
                  }
                ],
                "data" : null
              }
            """
          )
  
  test("Double-check that created roles hang around."):
    flaky():
      AsBob.withRoleRequest(RoleRequest.Admin).canonicalizeUser >>
      AsBob
        .queryRoleTypes
        .map: tpes =>
          assertEq(tpes, Set(RoleType.Pi, RoleType.Admin))

  test(s"Admin *can* call deleteRole"):
    flaky():
      AsBob.withRoleRequest(RoleRequest.Staff).canonicalizeUser >>
      AsBob.withRoleRequest(RoleRequest.Admin)
        .expectQueryWithUser(
          bob => s"""mutation { deleteRole(roleId: "${bob.role.id}") }""",
          json"""
            {
              "data" : {
                "deleteRole" : true
              }
            }
          """
        ) >>
      AsBob
        .queryRoleTypes
        .map: tpes =>
          assertEq(tpes, Set(RoleType.Pi, RoleType.Staff))


