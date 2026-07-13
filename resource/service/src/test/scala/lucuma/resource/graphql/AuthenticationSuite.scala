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

/**
 * Verifies that GraphQL endpoints are protected by authentication. The Resource service does not
 * authenticate users itself; it only validates JWTs and API keys. A request with valid SSO
 * credentials is served; one with missing or invalid credentials is rejected with 403 Forbidden.
 */
class AuthenticationSuite extends ResourceGraphQLSuite:

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

  test("A request with no credentials is forbidden"):
    expectFailure(timelineQuery, variables)

  test("A request with a malformed bearer token is forbidden"):
    expectFailure(
      timelineQuery,
      variables,
      authorization =
        rawAuthorization(Authorization(Credentials.Token(AuthScheme.Bearer, "not-a-real-jwt")))
    )

  test("A request with an unsupported Authorization scheme is forbidden"):
    expectFailure(
      timelineQuery,
      variables,
      authorization = rawAuthorization(Authorization(BasicCredentials("user", "password")))
    )
