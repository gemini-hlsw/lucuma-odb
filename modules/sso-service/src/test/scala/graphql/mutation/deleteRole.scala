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

object deleteRole extends GraphQLSuite with SsoSuite with Fixture with FlakyTests with OrcidIdGenerator[IO]:

  extension (as: As)      
    def queryRoleTypes: IO[Set[RoleType]] =
      as.query("query { user { roles { type } } }").map: json =>
        json.hcursor.downFields("data", "user", "roles").require[Set[Json]].map: j =>
          j.hcursor.downField("type").require[RoleType]
    def run: IO[Unit] =
      as.query("user { id }").void

  List(None, Some(RoleRequest.Staff), Some(RoleRequest.Ngo(Partner.CA))).foreach: rr =>
    test(s"${rr.getOrElse(RoleRequest.Pi).tpe} can't call deleteRole"):
      var bob: StandardUser = null // i'm sorry
      As(Bob, withRole = rr)
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
    randomOrcidId.flatMap: oid =>
      As(Bob, withOrcidId = Some(oid), withRole = Some(RoleRequest.Admin)).run >>
      As(Bob, withOrcidId = Some(oid))
        .queryRoleTypes
        .map: tpes =>
          expect.same(tpes, Set(RoleType.Pi, RoleType.Admin))

  test(s"Admin *can* call deleteRole"):
    randomOrcidId.flatMap: oid =>
      As(Bob, withOrcidId = Some(oid), withRole = Some(RoleRequest.Staff)).run >>
      As(Bob, withOrcidId = Some(oid), withRole = Some(RoleRequest.Admin))
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
      As(Bob, withOrcidId = Some(oid))
        .queryRoleTypes
        .map: tpes =>
          expect.same(tpes, Set(RoleType.Pi, RoleType.Staff))


