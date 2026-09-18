// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql

import io.circe.Json
import lucuma.resource.test.ResourceGraphQLSuite
import lucuma.resource.test.TestSso
import org.http4s.*
import org.http4s.headers.Authorization

class AuthenticationSuite extends ResourceGraphQLSuite:

  private def requiresAuth(field: String): List[String] =
    List(s"Field '$field' requires authentication.")

  private val dataQuery =
    """query {
      |  tooSupport(site: GN, start: "2026-03-01T00:00:00Z", end: "2026-03-02T00:00:00Z") {
      |    tooSupport
      |  }
      |}""".stripMargin

  test("A request with a valid JWT is authenticated and served"):
    expectSuccess(
      query = dataQuery,
      expected = Json.obj("tooSupport" -> Json.arr())
    )

  test("A request with a valid JWT for another user is also served"):
    expectSuccess(
      query = dataQuery,
      expected = Json.obj("tooSupport" -> Json.arr()),
      authorization = asUser(TestSso.standardUser(42, 420))
    )

  test("A data query with no credentials is rejected"):
    expect(
      dataQuery,
      Left(requiresAuth("tooSupport")),
      authorization = anonymous
    )

  test("A data query with a malformed bearer token is denied"):
    expect(
      dataQuery,
      Left(List("Access denied.")),
      authorization =
        rawAuthorization(Authorization(Credentials.Token(AuthScheme.Bearer, "not-a-real-jwt")))
    )

  test("A data query with an unsupported Authorization scheme is denied"):
    expect(
      dataQuery,
      Left(List("Access denied.")),
      authorization = rawAuthorization(Authorization(BasicCredentials("user", "password")))
    )
