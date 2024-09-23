// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Target

// https://github.com/gemini-hlsw/lucuma-odb/issues/307
class GitHub_307 extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def testIncludeDeleted(includeDeleted: Option[Boolean])(f: Target.Id => Json) =
    test(s"Query/targetGroup should respect includeDeleted flag ($includeDeleted)") {
      val setup =
        for {
          pid <- createProgramAs(pi)
          tid <- createTargetAs(pi, pid)
          oid <- createObservationAs(pi, pid, tid)
          _   <- deleteTargetAs(pi, tid)
        } yield (pid, tid)

      setup.flatMap { (pid, tid) =>
        expect(
          user = pi,
          query = s"""
          query {
            targetGroup(programId: "$pid" ${includeDeleted.foldMap(b => s"includeDeleted: $b")}) {
              matches {
                target {
                  id
                  existence
                }
              }
            }
          }
          """,
          expected = Right(f(tid))
        )
      }
    }

  testIncludeDeleted(Some(true)) { tid =>
    json"""
      {
        "targetGroup" : {
          "matches" : [
            {
              "target" : {
                "id" : $tid,
                "existence" : "DELETED"
              }
            }
          ]
        }
      }
    """
  }

  testIncludeDeleted(Some(false)) { _ =>
    json"""
      {
        "targetGroup" : {
          "matches" : [
          ]
        }
      }
    """
  }

  testIncludeDeleted(None) { _ =>
    json"""
      {
        "targetGroup" : {
          "matches" : [
          ]
        }
      }
    """
  }

}
