package lucuma.odb.graphql
package query

import cats.syntax.all._
import io.circe.literal._
import lucuma.odb.graphql.OdbSuite

class filterTypeMeta extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  def validUsers = List(pi, guest, service)

  test("all users should have access") {
    validUsers.traverse { user =>
      expect(
        user  = user,
        query =
          """
            query {
              filterTypeMeta {
                tag
                shortName
                longName
              }
            }
          """,
        expected =
          Right(
            json"""
              {
                "filterTypeMeta": [
                  {
                    "tag": "BROAD_BAND",
                    "shortName": "Broad-Band",
                    "longName": "Broad-Band Filter"
                  },
                  {
                    "tag": "NARROW_BAND",
                    "shortName": "Narrow-Band",
                    "longName": "Narrow-Band Filter"
                  },
                  {
                    "tag": "COMBINATION",
                    "shortName": "Combination",
                    "longName": "Combination Filter"
                  },
                  {
                    "tag": "SPECTROSCOPIC",
                    "shortName": "Spectroscopic",
                    "longName": "Spectroscopic Filter"
                  },
                  {
                    "tag": "ENGINEERING",
                    "shortName": "Engineering",
                    "longName": "Engineering Filter"
                  }
                ]
              }
            """
          )
      )
    }
  }

}