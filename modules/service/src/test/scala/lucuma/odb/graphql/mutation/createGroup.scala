// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.literal._

class createGroup extends OdbSuite {

  val pi = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers = List(pi)

  test("simple group creation") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            createGroup(
              input: {
                programId: "$pid",
                SET: {
                  name: "My Group"
                  description: "A description"
                  minimumRequired: 4
                  ordered: true
                  minimumInterval: {
                    hours: 3
                  }
                  maximumInterval: {
                    hours: 36
                  }
                }
              }
            ) {
              group {
                name
                description
                minimumRequired
                ordered
                minimumInterval {
                  hours
                }
                maximumInterval {
                  hours
                }
                elements {
                  parentGroupId
                  parentIndex
                  group { id }
                  observation { id }
                }
              }
            }
          }
        """,
        expected = Right(json"""
          {
            "createGroup" : {
              "group" : {
                "name" : "My Group",
                "description" : "A description",
                "minimumRequired" : 4,
                "ordered" : true,
                "minimumInterval" : {
                  "hours" : 3.000000
                },
                "maximumInterval" : {
                  "hours" : 36.000000
                },
                "elements" : [
                ]
              }
            }
          }
        """)
      )
    }
  }

  test("[program] create many groups and then select them (in order)") {
    for {
      pid  <- createProgramAs(pi)
      g1   <- createGroupAs(pi, pid)
      g2   <- createGroupAs(pi, pid)
      g3   <- createGroupAs(pi, pid)
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Left(g1), Left(g2), Left(g3)))
  }

  test("[program] insert group at beginning") {
    for {
      pid  <- createProgramAs(pi)
      g1   <- createGroupAs(pi, pid)
      g2   <- createGroupAs(pi, pid)
      g3   <- createGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Left(g3), Left(g1), Left(g2)))
  }

  test("[program] insert group in the middle") {
    for {
      pid  <- createProgramAs(pi)
      g1   <- createGroupAs(pi, pid)
      g2   <- createGroupAs(pi, pid)
      g3   <- createGroupAs(pi, pid, None, Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, None)
    } yield assertEquals(ids, List(Left(g1), Left(g3), Left(g2)))
  }

  test("[group] create many sub-groups and then select them (in order)") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      g1   <- createGroupAs(pi, pid, Some(gid))
      g2   <- createGroupAs(pi, pid, Some(gid))
      g3   <- createGroupAs(pi, pid, Some(gid))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Left(g1), Left(g2), Left(g3)))
  }

  test("[group] insert sub-group at beginning") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      g1   <- createGroupAs(pi, pid, Some(gid))
      g2   <- createGroupAs(pi, pid, Some(gid))
      g3   <- createGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(0)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Left(g3), Left(g1), Left(g2)))
  }

  test("[group] insert sub-group in the middle") {
    for {
      pid  <- createProgramAs(pi)
      gid  <- createGroupAs(pi, pid)
      g1   <- createGroupAs(pi, pid, Some(gid))
      g2   <- createGroupAs(pi, pid, Some(gid))
      g3   <- createGroupAs(pi, pid, Some(gid), Some(NonNegShort.unsafeFrom(1)))
      ids  <- groupElementsAs(pi, pid, Some(gid))
    } yield assertEquals(ids, List(Left(g1), Left(g3), Left(g2)))
  }

}
