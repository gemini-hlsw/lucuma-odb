// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package `export`

import cats.effect.IO
import org.http4s.client.JavaNetClientBuilder

class enumMetadata extends OdbSuite {

  val validUsers = Nil
  val client = JavaNetClientBuilder[IO].create

  test("enumMetadata") {
    server.use { svr =>
      client.expect[String](svr.baseUri / "export" / "enumMetadata").map { s =>
        assert(s.startsWith("export const enumMetadata ='{"))
      }
    }
  }

}


