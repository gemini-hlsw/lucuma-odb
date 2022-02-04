package lucuma.odb.graphql

import cats.syntax.all._
import io.circe.literal._
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.graphql.OdbSuite

class EnumMetaSuite extends OdbSuite {

  // we need to generate a random user here and then tear the user down on completion

  override def showContainerLog: Boolean = true

  lazy val user = StandardUser(
    id         = Gid[User.Id].fromString.getOption("u-107").get,
    role       = StandardRole.Pi(Gid[StandardRole.Id].fromString.getOption("r-100").get), // unused
    otherRoles = Nil,
    profile    = OrcidProfile(
      orcidId      = OrcidId.fromValue("0000-0003-1301-6629").toOption.get,
      givenName    = Some("Rob"),
      familyName   = Some("Norris"),
      creditName   = Some("Testing!"),
      primaryEmail = Some(""),
    )
  ).some

  queryTest(
    name  = "filterTypeMeta",
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