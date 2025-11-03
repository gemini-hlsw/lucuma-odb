// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.Group

class group extends OdbSuite {

  val pi0 = TestUsers.Standard.pi(1, 30)
  val pi1 = TestUsers.Standard.pi(2, 30)
  val validUsers = List(pi0, pi1)

  private def queryGroup(gid: Group.Id): IO[Group.Id] =
    query(
      pi0,
      s"""query { group(groupId: "$gid") { id } }"""
    ).flatMap {
       _.hcursor
        .downFields("group", "id")
        .as[Group.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
     }

  test("can select a group by id") {
    for {
      pid <- createProgramAs(pi0)
      gex <- createGroupAs(pi0, pid)
      gob <- queryGroup(gex)
    } yield assertEquals(gob, gex)
  }

  test("group may not be found") {
    expect(pi0,
      s"""query { group(groupId: "${Group.Id.fromLong(42L).get}") { id } }""",
      json"""{ "group": null }""".asRight
    )
  }

  test("group may not be visible") {
    for {
      pid <- createProgramAs(pi0)
      gid <- createGroupAs(pi0, pid)
      _   <- expect(pi1,  // not visible to pi1
               s"""query { group(groupId: "$gid") { id } }""",
               json"""{ "group": null }""".asRight
             )
    } yield ()
  }

  test("groups have a system property") {
    for {
      pid <- createProgramAs(pi0)
      gid <- createGroupAs(pi0, pid)
      _   <- expect(pi0,  // not visible to pi1
               s"""query { group(groupId: "$gid") { id system } }""",
               json"""{ "group": {"id": $gid, "system": false } }""".asRight
             )
    } yield ()
  }

}
