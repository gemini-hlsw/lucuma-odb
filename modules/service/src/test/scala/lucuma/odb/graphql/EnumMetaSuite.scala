package lucuma.odb.graphql

import cats.data.NonEmptyList
import io.circe.literal._
import lucuma.odb.graphql.OdbSuite

class EnumMetaSuite extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val guest    = TestUsers.guest(2)
  val service  = TestUsers.service(3)

  val allUsers = NonEmptyList.of(pi, guest, service)

  queryTest(
    name  = "filterTypeMeta",
    users = allUsers,
    query = """
      query {
        filterTypeMeta {
          tag
          shortName
          longName
        }
      }
    """,
    expected = json"""
      {
        "filterTypeMeta": [
          {
            "tag": "BroadBand",
            "shortName": "Broad-Band",
            "longName": "Broad-Band Filter"
          },
          {
            "tag": "NarrowBand",
            "shortName": "Narrow-Band",
            "longName": "Narrow-Band Filter"
          },
          {
            "tag": "Combination",
            "shortName": "Combination",
            "longName": "Combination Filter"
          },
          {
            "tag": "Spectroscopic",
            "shortName": "Spectroscopic",
            "longName": "Spectroscopic Filter"
          },
          {
            "tag": "Engineering",
            "shortName": "Engineering",
            "longName": "Engineering Filter"
          }
        ]
      }
    """
  )

  queryTest(
    name  = "partnerMeta",
    users = allUsers,
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
    expected = json"""
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

}