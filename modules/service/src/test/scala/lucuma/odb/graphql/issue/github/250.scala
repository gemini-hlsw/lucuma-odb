// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User

class GitHub_250 extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def deleteTarget(user: User, tid: Target.Id): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: {
              existence: DELETED
            }
            WHERE: {
              id: { EQ: "$tid"}
            }
          }) {
            targets {
              id
            }
          }
        }
      """
    ).void

  def renameTarget(user: User, tid: Target.Id, name: String): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: {
              name: "$name"
            }
            WHERE: {
              id: { EQ: "$tid"}
            }
          }) {
            targets {
              id
            }
          }
        }
      """
    ).void

  test("observation title should reflect target rename") {
    for {
      pid  <- createProgramAs(pi)
      tids <- List("foo", "bar", "baz", "qux", "quux").traverse(createTargetAs(pi, pid, _))
      oid1 <- createObservationAs(pi, pid, tids(0), tids(1))
      oid2 <- createObservationAs(pi, pid, tids(1), tids(2), tids(3))
      oid3 <- createObservationAs(pi, pid, tids(2))
      oid4 <- createObservationAs(pi, pid)
      _    <- renameTarget(pi, tids(1), "fnord")
      _  <- expect(
        user = pi,
        query =
          s"""
            query {
              observations(WHERE: {
                program: {
                  id: { EQ: "$pid" }
                }
              }) {
                matches {
                  id
                  title
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "observations" : {
                "matches" : [
                  {
                    "id" : $oid1,
                    "title" : "foo, fnord"
                  },
                  {
                    "id" : $oid2,
                    "title" : "fnord, baz, qux"
                  },
                  {
                    "id" : $oid3,
                    "title" : "baz"
                  },
                  {
                    "id" : $oid4,
                    "title" : "Untargeted"
                  }
                ]
              }
            }"""
        )
      )
    } yield true
  }

  test("observation title should reflect target deletion") {
    for {
      pid  <- createProgramAs(pi)
      tids <- List("foo", "bar", "baz", "qux", "quux").traverse(createTargetAs(pi, pid, _))
      oid1 <- createObservationAs(pi, pid, tids(0), tids(1))
      oid2 <- createObservationAs(pi, pid, tids(1), tids(2), tids(3))
      oid3 <- createObservationAs(pi, pid, tids(2))
      oid4 <- createObservationAs(pi, pid)
      _    <- deleteTarget(pi, tids(1))
      _  <- expect(
        user = pi,
        query =
          s"""
            query {
              observations(WHERE: {
                program: {
                  id: { EQ: "$pid" }
                }
              }) {
                matches {
                  id
                  title
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "observations" : {
                "matches" : [
                  {
                    "id" : $oid1,
                    "title" : "foo"
                  },
                  {
                    "id" : $oid2,
                    "title" : "baz, qux"
                  },
                  {
                    "id" : $oid3,
                    "title" : "baz"
                  },
                  {
                    "id" : $oid4,
                    "title" : "Untargeted"
                  }
                ]
              }
            }"""
        )
      )
    } yield true
  }

}
