// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid
import lucuma.odb.data.PartnerLink

class updateProgramUsers extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val guest1  = TestUsers.guest(3)
  val guest2  = TestUsers.guest(4)
  val staff   = TestUsers.Standard.staff(5, 34)

  val piCharles = TestUsers.Standard(
    6,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(6).get),
    primaryEmail = "charles@guiteau.com".some
  )

  val piLeon    = TestUsers.Standard(
    7,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(7).get),
    primaryEmail = "leon@czolgosz.edu".some
  )

  val service = TestUsers.service(10)

  val validUsers = List(pi, pi2, guest1, guest2, staff, piCharles, piLeon, service).toList

  test("simple update pi") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  partnerLink: {
                    partner: US
                  }
                }
                WHERE: {
                  user: {
                    id: { EQ: "${pi.id}" }
                  }
                }
              }
            ) {
              hasMore
              programUsers {
                program { id }
                user { id }
                partnerLink {
                  linkType
                  ... on HasPartner {
                    partner
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(Json.obj(
            "updateProgramUsers" -> Json.obj(
              "hasMore" -> Json.False,
              "programUsers" -> Json.arr(
                Json.obj(
                  "program" -> Json.obj("id" -> pid.asJson),
                  "user"    -> Json.obj("id" -> pi.id.asJson),
                  "partnerLink" -> Json.obj(
                    "linkType" -> PartnerLink.LinkType.HasPartner.tag.toScreamingSnakeCase.asJson,
                    "partner"  -> Partner.US.tag.toScreamingSnakeCase.asJson
                  )
                )
              )
            )
          )
        )
      )
    }
  }

}
