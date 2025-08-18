// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.implicits.*
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.*
import lucuma.core.util.Gid
import lucuma.sso.client.ApiKey
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.database.RoleRequest.Admin
import lucuma.sso.service.orcid.OrcidIdGenerator
import lucuma.sso.service.orcid.OrcidPerson
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.Credentials
import org.http4s.Headers
import org.http4s.Method
import org.http4s.QueryParamEncoder
import org.http4s.Request
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.headers.Location
import org.typelevel.ci.CIString
import weaver.Expectations

object GraphQLSuite extends SsoSuite with Fixture with FlakyTests with OrcidIdGenerator[IO] {

  implicit class ACursorOps(hc: ACursor) {
    def downFields(fields: String*): ACursor = fields.foldLeft(hc)(_ downField _)
    def require[A: Decoder]: A = hc.as[A].fold(e => sys.error(e.message), identity)
  }

  implicit def gidQueryParamEncoder[A: Gid]: QueryParamEncoder[A] =
    QueryParamEncoder[String].contramap(Gid[A].fromString.reverseGet)

  def doQueryAs(user: StandardUser, query: String, sso: Client[IO]): IO[Json] =
    for
      jwt <- sso.expect[String](Request[IO](method = Method.POST, uri = SsoRoot / "api" / "v1" / "refresh-token"))

      // Create an API Key
      apiKey <- sso.expect[ApiKey](
        Request[IO](
          method  = Method.POST,
          uri     = (SsoRoot / "api" / "v1" / "create-api-key").withQueryParam("role", user.role.id),
          headers = Headers(Authorization(Credentials.Token(CIString("Bearer"), jwt))),
        )
      )

      // Run a query!
      result <- sso.fetchAs[Json](
        Request[IO](
          method  = Method.POST,
          uri     = SsoRoot / "graphql",
          headers = Headers(Authorization(Credentials.Token(CIString("Bearer"), ApiKey.fromString.reverseGet(apiKey)))),
        ).withEntity(
          Json.obj(
            "query" -> Json.fromString(query)
          )
        )
      )
    yield result

  def queryAs(person: OrcidPerson, query: StandardUser => String, extraRole: Option[RoleRequest | StandardRole.Id], withOrcidId: Option[OrcidId]): IO[Json] =
    def setRole(rid: StandardRole.Id)  = (SsoRoot / "auth" / "v1" / "set-role").withQueryParam("role", rid.toString)
    SsoSimulator[IO].use: (db, sim, sso, _, _) =>
      val stage1  = (SsoRoot / "auth" / "v1" / "stage1").withQueryParam("state", ExploreRoot)
      for
        redir  <- sso.get(stage1)(_.headers.get[Location].map(_.uri).get.pure[IO])
        stage2 <- sim.authenticate(redir, person, withOrcidId)
        tok1   <- sso.get(stage2)(CookieReader[IO].getSessionToken) // ensure we have our cookie
        user1  <- db.use(_.getStandardUserFromToken(tok1))
        user2  <-
          extraRole match
            case None => user1.pure
            case Some(rr: RoleRequest) => 
              for
                rid    <- db.use(_.canonicalizeRole(user1, rr))
                tok2   <- sso.get(setRole(rid))(CookieReader[IO].getSessionToken)
                user2  <- db.use(_.getStandardUserFromToken(tok2))
              yield user2
            case Some(rid: StandardRole.Id) => 
              for
                tok2   <- sso.get(setRole(rid))(CookieReader[IO].getSessionToken)
                user2  <- db.use(_.getStandardUserFromToken(tok2))
              yield user2
        result <- doQueryAs(user2, query(user2), sso)
      yield result
    .onError(e => IO(println(e)))

  case class As(person: OrcidPerson, withOrcidId: Option[OrcidId] = None, withRole: Option[RoleRequest | StandardRole.Id] = None):

    def query(query: String): IO[Json] =
      queryWithUser(_ => query)

    def queryWithUser(query: StandardUser => String): IO[Json] =
      queryAs(person, query, withRole, withOrcidId)

    def expectQuery(query: String, expected: Json): IO[Expectations] =
      expectQueryWithUser(_ => query, expected)

    def expectQueryWithUser(query: StandardUser => String, expected: Json): IO[Expectations] =
      queryAs(person, query, withRole, withOrcidId).map: result =>
        if result != expected then
          println(s"Result: $result\n\nExpected: $expected")
        expect(result == expected)

    def queryIds: IO[(User.Id, OrcidId)] =
      query("query { role { user { id orcidId }}}")
        .map: json =>
          (
            json.hcursor.downFields("data", "role", "user", "id").require[User.Id],
            json.hcursor.downFields("data", "role", "user", "orcidId").require[OrcidId],
          )

  test("Query current role.") {
    flaky()(
      As(Bob).expectQuery(
        query = """
          query {
            role {
              type
              partner
              user {
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
        expected = json"""{
          "data" : {
            "role" : {
              "type" : "PI",
              "partner" : null,
              "user" : {
                "profile": {
                  "givenName" : "Bob",
                  "familyName" : "Dobbs",
                  "creditName" : null,
                  "email" : "bob@dobbs.com"
                }
              }
            }
          }
        }"""
      )
    )
  }

  test("Attempt to create API key with invalid role id") {
    flaky()(
      As(Bob).expectQuery(
        query = """
          mutation {
            createApiKey(role: "bogus")
          }
        """,
        expected = json"""{
          "errors" : [
            {
              "message" : "Not a valid role id: bogus"
            }
          ]
        }"""
      )
    )
  }

  test("Attempt to create API key with a role we don't own") {
    flaky()(
      As(Bob).expectQuery(
        query = """
          mutation {
            createApiKey(role: "r-99999")
          }
        """,
        expected = json"""{
          "errors" : [
            {
              "message" : "No such role: r-99999"
            }
          ],
          "data" : null
        }"""
      )
    )
  }

  test("Create an API key") {
    flaky()(
      As(Bob).queryWithUser { user =>
        show"""
          mutation {
            createApiKey(role: "${user.role.id}")
          }
        """
      } .map { json =>
        expect(
          json.hcursor
          .downFields("data", "createApiKey")
          .as[String]
          .toOption
          .map(ApiKey.fromString.getOption)
          .isDefined
        )
      }
    )
  }

  List(RoleRequest.Staff, RoleRequest.Ngo(Partner.AR)).foreach: role =>
    test(s"$role should not be able to give Bob a role"):
      flaky():
        As(Bob)
          .queryIds
          .flatMap: (bob, _) =>
            As(Alice)
              .queryIds
              .flatMap: 
                case (alice, aliceOrcid) =>
                  As(Alice, Some(aliceOrcid), Some(role))
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
          As(Alice, None, Some(Admin))
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
              expect:
                json
                  .hcursor
                  .downFields("data", "addRole")
                  .as[StandardRole.Id]
                  .isRight


  test("Admin Alice should be able to give Bob an Admin role, and aftewards he should be able to give a Staff role to Alice"):
    flaky():
      As(Bob)
        .queryIds
        .flatMap: (bob, bobOrcid) =>
          As(Alice)
            .queryIds
            .flatMap: (alice, aliceOrcid) =>
              As(Alice, Some(aliceOrcid), Some(Admin))
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
                  As(Bob, Some(bobOrcid), Some(newRole))
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
                      expect:
                        json
                          .hcursor
                          .downFields("data", "addRole")
                          .as[StandardRole.Id]
                          .isRight  

}