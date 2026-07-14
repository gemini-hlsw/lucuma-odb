// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql

import io.circe.Json
import lucuma.resource.test.ResourceGraphQLSuite

class IntrospectionSuite extends ResourceGraphQLSuite:

  private val introspectionQuery =
    """query {
      |  __schema {
      |    queryType { name }
      |  }
      |}""".stripMargin

  test("introspection query succeeds without authentication"):
    expectSuccess(
      query = introspectionQuery,
      expected = Json.obj(
        "__schema" -> Json.obj(
          "queryType" -> Json.obj("name" -> Json.fromString("Query"))
        )
      ),
      authorization = anonymous
    )
