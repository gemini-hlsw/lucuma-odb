// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.refined.*

class cloneTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers = List(pi, pi2)

  test("simple clone") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
              }) {
                originalTarget $FullTargetGraph
                newTarget $FullTargetGraph

                originalTargetId: originalTarget { id }
                newTargetId: newTarget { id }
              }
            }
          """
        ).flatMap { json =>

          // The data fields (i.e., everything but ID) should be the same
          assertEquals(
            json.hcursor.downFields("cloneTarget", "originalTarget").as[Json],
            json.hcursor.downFields("cloneTarget", "newTarget").as[Json]
          )

          // The ids should exist, so we'll just 'get' them
          val origId = json.hcursor.downFields("cloneTarget", "originalTargetId", "id").as[Target.Id].toOption.get
          val newId =  json.hcursor.downFields("cloneTarget", "newTargetId", "id").as[Target.Id].toOption.get

          // The ids should be different
          assertNotEquals(origId, newId)

          // The target roles should match
          (getCalibrationRoleFromDb(origId), getCalibrationRoleFromDb(newId)).parMapN((oldCalib, newCalib) =>
            assertEquals(oldCalib, newCalib)
          )
        }
      }
    }
  }

  test("clone with rename") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
                SET: {
                  name: "New Name"
                }
              }) {
                originalTarget {
                  name
                }
                newTarget {
                  name
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "cloneTarget" : {
                  "originalTarget" : {
                    "name" : "My Target"
                  },
                  "newTarget" : {
                    "name" : "New Name"
                  }
                }
              }
            """
          )
        )
      }
    }
  }

  test("clone with bogus target id") {
    expect(
      user = pi,
      query = s"""
        mutation {
          cloneTarget(input: {
            targetId: "t-ffff"
          }) {
            newTarget {
              id
            }
          }
        }
      """,
      expected = Left(List("No such target: t-ffff"))
    )
  }

  test("clone with bogus update") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
                SET: {
                  sourceProfile: {
                    gaussian: {
                    }
                  }
                }
              }) {
                newTarget {
                  id
                }
              }
            }
          """,
          expected = Left(List("Not a gaussian source.  To change profile type, please provide a full definition."))
        )
      }
    }
  }

  test("clone someone else's target") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi2, // different user!
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
              }) {
                newTarget {
                  id
                }
              }
            }
          """,
          expected = Left(List(s"No such target: $tid"))
        )
      }
    }
  }

  test("clone a calibration target") {
    createProgramAs(pi).flatMap { pid =>
      createCalibrationTargetIn(pid, "Estrella Guía".refined).flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
              }) {
                originalTarget {
                  id
                }
              }
            }
          """,
          expected = Right(
            json"""{
                "cloneTarget" : {
                  "originalTarget" :
                    {
                      "id" : $tid
                    }
                }
              }
            """
          )
        )
      }
    }
  }

  test("clone and replace in an observation") {

    def cloneTarget(tid: Target.Id, oids: List[Observation.Id]): IO[Target.Id] =
      query(pi, s"""
        mutation {
          cloneTarget(input: {
            targetId: "$tid"
            REPLACE_IN: ${oids.asJson}
          }) {
            newTarget { id }
          }
        }
      """).map(_.hcursor.downFields("cloneTarget", "newTarget", "id").require[Target.Id])

    def asterism(oid: Observation.Id): IO[List[Target.Id]] =
      query(pi, s"""
        query {
          observation(observationId: "$oid") {
            targetEnvironment {
              asterism {
                id
              }
            }
          }
        }
      """).map {
        _.hcursor.downFields("observation", "targetEnvironment", "asterism").require[List[Json]]
         .map(_.hcursor.downField("id").require[Target.Id])
      }

    for {
      // one program
      pid  <- createProgramAs(pi)
      // several targets
      tid1 <- createTargetAs(pi, pid, "Target 1")
      tid2 <- createTargetAs(pi, pid, "Target 2")
      tid3 <- createTargetAs(pi, pid, "Target 3")
      // several obs with both targets
      oid1 <- createObservationAs(pi, pid, tid1, tid3) // 1, 3
      oid2 <- createObservationAs(pi, pid, tid1, tid3) // 1, 3
      oid3 <- createObservationAs(pi, pid, tid2, tid3) // 2, 3
      // clone a target and replace it in some obs
      tid4 <- cloneTarget(tid3, List(oid2, oid3))
      // fetch our asterisms
      ast1 <- asterism(oid1)
      ast2 <- asterism(oid2)
      ast3 <- asterism(oid3)
    } yield {
      assertEquals(ast1, List(tid1, tid3)) // unchanged
      assertEquals(ast2, List(tid1, tid4)) // 1, 3 -> 4
      assertEquals(ast3, List(tid2, tid4)) // 2, 3 -> 4
    }
  }

}
