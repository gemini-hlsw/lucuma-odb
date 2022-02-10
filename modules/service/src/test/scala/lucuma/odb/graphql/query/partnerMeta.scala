package lucuma.odb.graphql
package query

import cats.syntax.all._
import io.circe.literal._
import lucuma.odb.graphql.OdbSuite

class partnerMeta extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  def validUsers = List(pi, guest, service)

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
                  },
                  {
                    "tag" : "GT",
                    "shortName" : "Guaranteed Time",
                    "longName" : "Guaranteed Time",
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