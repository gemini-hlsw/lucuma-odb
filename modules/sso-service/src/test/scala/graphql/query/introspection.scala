// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import cats.effect.IO
import io.circe.Json
import lucuma.sso.service.simulator.SsoSimulator
import org.http4s.Method
import org.http4s.Request
import org.http4s.circe.*

object introspection extends GraphQLSuite with SsoSuite with Fixture:

  test("Schema introspection succeeds without authentication."):
    SsoSimulator[IO].use: (_, _, sso, _, _) =>
      sso
        .fetchAs[Json](
          Request[IO](method = Method.POST, uri = SsoRoot / "graphql").withEntity(
            Json.obj(
              "query" -> Json.fromString("query { __schema { queryType { name } } }")
            )
          )
        )
        .map: json =>
          val name =
            json.hcursor.downFields("data", "__schema", "queryType", "name").require[String]
          expect.same("Query", name)
