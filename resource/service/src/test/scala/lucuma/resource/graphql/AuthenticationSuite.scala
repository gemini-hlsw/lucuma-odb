// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.resource.test.ResourceGraphQLSuite
import lucuma.resource.test.TestSso
import org.http4s.*
import org.http4s.headers.Authorization

class AuthenticationSuite extends ResourceGraphQLSuite:

  private def requiresAuth(field: String): List[String] =
    List(s"Field '$field' requires authentication.")

  private val timelineQuery =
    """query($site: Site!) {
      |  telescopeNightTimeline(site: $site, observingNight: "2026-08-01") {
      |    site
      |  }
      |}""".stripMargin

  private val variables: Option[JsonObject] =
    JsonObject("site" -> Site.GN.asJson).some

  test("A request with a valid JWT is authenticated and served"):
    expectSuccess(
      query = timelineQuery,
      expected = Json.obj("telescopeNightTimeline" -> Json.Null),
      variables = variables
    )

  test("A request with a valid JWT for another user is also served"):
    expectSuccess(
      query = timelineQuery,
      expected = Json.obj("telescopeNightTimeline" -> Json.Null),
      variables = variables,
      authorization = asUser(TestSso.standardUser(42, 420))
    )

  test("A data query with no credentials is rejected"):
    expect(
      timelineQuery,
      Left(requiresAuth("telescopeNightTimeline")),
      variables,
      authorization = anonymous
    )

  test("A data query with a malformed bearer token is rejected"):
    expect(
      timelineQuery,
      Left(requiresAuth("telescopeNightTimeline")),
      variables,
      authorization =
        rawAuthorization(Authorization(Credentials.Token(AuthScheme.Bearer, "not-a-real-jwt")))
    )

  test("A data query with an unsupported Authorization scheme is rejected"):
    expect(
      timelineQuery,
      Left(requiresAuth("telescopeNightTimeline")),
      variables,
      authorization = rawAuthorization(Authorization(BasicCredentials("user", "password")))
    )
