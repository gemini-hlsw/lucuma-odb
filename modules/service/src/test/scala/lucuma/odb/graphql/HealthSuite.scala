// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import lucuma.core.model.User
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status

// Guards the mounting: /health must stay outside the auth middleware.
class HealthSuite extends OdbSuite:

  val validUsers: List[User] = Nil

  test("GET /health needs no credentials"):
    val res =
      for
        svr    <- server
        client <- httpClientResource
        res    <- client.run(Request[IO](Method.GET, svr.baseUri / "health"))
      yield res
    res.use(r => IO(assertEquals(r.status, Status.Ok)))
