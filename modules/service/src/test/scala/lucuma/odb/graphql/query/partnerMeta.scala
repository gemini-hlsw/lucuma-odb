// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.literal.*

class partnerMeta extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  def validUsers = List(pi, guest, service)

  test("enum should be correct") {
    expect(
      user = pi,
      query = """
        query {
          __type(name: "Partner") {
            kind
            enumValues {
              name
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "__type": {
              "kind": "ENUM",
              "enumValues": [
                {
                  "name": "AR"
                },
                {
                  "name": "BR"
                },
                {
                  "name": "CA"
                },
                {
                  "name": "CFH"
                },
                {
                  "name": "CL"
                },
                {
                  "name": "GT"
                },
                {
                  "name": "KECK"
                },
                {
                  "name": "KR"
                },
                {
                  "name": "LP"
                },
                {
                  "name": "SUBARU"
                },
                {
                  "name": "UH"
                },
                {
                  "name": "US"
                }
              ]
            }
          }
        """
      )
    )
  }

  test("all users should have access") {
    validUsers.traverse { user =>
      expect(
        user  = user,
        query = """
          query {
            partnerMeta {
              tag
              shortName
              longName
              active
            }
          }
        """,
        expected =
          Right(
            json"""
              {
                "partnerMeta" : [
                  {
                    "tag" : "AR",
                    "shortName" : "Argentina",
                    "longName" : "Argentina",
                    "active" : true
                  },
                  {
                    "tag" : "BR",
                    "shortName" : "Brazil",
                    "longName" : "Brazil",
                    "active" : true
                  },
                  {
                    "tag" : "CA",
                    "shortName" : "Canada",
                    "longName" : "Canada",
                    "active" : true
                  },
                  {
                    "tag" : "CFH",
                    "shortName" : "CFHT",
                    "longName" : "Canada-France-Hawaii Telescope",
                    "active" : true
                  },
                  {
                    "tag" : "CL",
                    "shortName" : "Chile",
                    "longName" : "Chile",
                    "active" : true
                  },
                  {
                    "tag" : "GT",
                    "shortName" : "Guaranteed Time",
                    "longName" : "Guaranteed Time",
                    "active" : true
                  },
                  {
                    "tag" : "KECK",
                    "shortName" : "Keck",
                    "longName" : "Keck Observatory",
                    "active" : true
                  },
                  {
                    "tag" : "KR",
                    "shortName" : "Korea",
                    "longName" : "Republic of Korea",
                    "active" : true
                  },
                  {
                    "tag" : "LP",
                    "shortName" : "Long Programs",
                    "longName" : "Long Programs",
                    "active" : true
                  },
                  {
                    "tag" : "SUBARU",
                    "shortName" : "Subaru",
                    "longName" : "Subaru Telescope",
                    "active" : true
                  },
                  {
                    "tag" : "UH",
                    "shortName" : "University of Hawaii",
                    "longName" : "University of Hawaii",
                    "active" : true
                  },
                  {
                    "tag" : "US",
                    "shortName" : "United States",
                    "longName" : "United States",
                    "active" : true
                  }
                ]
              }
            """
          )
        )
    }
  }

}
