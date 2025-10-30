// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Group
import lucuma.odb.data.Existence
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

class group extends OdbSuite with DatabaseOperations {

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

  test("selectGroups with existence filter for observations") {
    withServices(pi0) { services =>
      services.transactionally {
        for {
          pid  <- createProgramAs(pi0)
          gid  <- createGroupAs(pi0, pid)
          oid1 <- createObservationInGroupAs(pi0, pid, Some(gid))
          oid2 <- createObservationInGroupAs(pi0, pid, Some(gid))
          oid3 <- createObservationInGroupAs(pi0, pid, Some(gid))
          // Delete two observations
          _    <- setObservationExistence(pi0, oid2, Existence.Deleted) *>
                    setObservationExistence(pi0, oid3, Existence.Deleted)
          tree <- services
                    .groupService(emailConfig, httpClient)
                    .selectGroups(
                      pid,
                      obsFilter = sql"c_existence = $existence"(Existence.Deleted)
                    )
          count = tree.collectObservations(_ => true).map(_._2.size).sum
         } yield assertEquals(count, 2)
      }
    }
  }

  test("selectGroups with groupFilter") {
    withServices(pi0) { services =>
      services.transactionally {
        for {
          pid  <- createProgramAs(pi0)
          gid1 <- createGroupAs(pi0, pid)
          gid2 <- createGroupAs(pi0, pid, name = "group".some)
          tree <- services
                    .groupService(emailConfig, httpClient)
                    .selectGroups(
                      pid,
                      groupFilter = sql"c_name = $varchar".apply("group")
                    )
        } yield assertEquals(tree.collectObservations(_ => true).size, 1)
      }
    }
  }

  test("selectGroups with both filters") {
    withServices(pi0) { services =>
      services.transactionally {
        for {
          pid        <- createProgramAs(pi0, "Test Program")
          gid1       <- createGroupAs(pi0, pid)
          gid2       <- createGroupAs(pi0, pid, name = "group2".some)
          presentObs <- createObservationInGroupAs(pi0, pid, Some(gid1))
          deletedObs <- createObservationInGroupAs(pi0, pid, Some(gid2))
          _          <- setObservationExistence(pi0, deletedObs, Existence.Deleted)
          // selectGroups with both filters: group 2 + deleted observations
          tree <-    services
                        .groupService(emailConfig, httpClient)
                        .selectGroups(
                          pid,
                          groupFilter = sql"c_name = $varchar".apply("group2"),
                          obsFilter = sql"c_existence = $existence"(Existence.Deleted)
                        )

        } yield {
          val groupCount = tree.collectObservations(_ => true).size
          val obsCount = tree.collectObservations(_ => true).map(_._2.size).sum
          assertEquals(groupCount, 1)
          assertEquals(obsCount, 1)
        }
      }
    }
  }
}
