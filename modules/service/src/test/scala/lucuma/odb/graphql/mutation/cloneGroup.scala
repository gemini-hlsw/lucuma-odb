// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.literal.*
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class cloneGroup extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val validUsers: List[User] = List(pi)

  test("simple clone of empty top-level group") {
    createProgramAs(pi).flatMap: pid =>
      createGroupAs(pi, pid) >> createGroupAs(pi, pid, None, None, Some(NonNegShort.unsafeFrom(42))).flatMap: gid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneGroup(input: {
                groupId: "$gid"
              }) {
                originalGroup {
                  parentIndex
                  minimumRequired
                }
                newGroup {
                  parentIndex
                  minimumRequired
                }
              }
            }
          """,
          expected = Right(json"""
            {
              "cloneGroup" : {
                "originalGroup" : {
                  "parentIndex" : 2,
                  "minimumRequired" : 42
                },
                "newGroup" : {
                  "parentIndex" : 1,
                  "minimumRequired" : 42
                }
              }
            }
          """)
        )      
  }

  test("clone with index should insert where requested") {
    createProgramAs(pi).flatMap: pid =>
      (createGroupAs(pi, pid) <* createGroupAs(pi, pid)).flatMap: gid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneGroup(input: {
                groupId: "$gid",
                SET: {
                  parentGroupIndex: 2
                }
              }) {
                originalGroup {
                  parentIndex
                }
                newGroup {
                  parentIndex
                }
              }
            }
          """,
          expected = Right(json"""
            {
              "cloneGroup" : {
                "originalGroup" : {
                  "parentIndex" : 0
                },
                "newGroup" : {
                  "parentIndex" : 2
                 }
              }
            }
          """)
        )      
  }

  test("clone of top-level group with things inside") {
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- createObservationInGroupAs(pi, pid, Some(gid))
      _   <- createGroupAs(pi, pid, Some(gid))        
      clo <- cloneGroupAs(pi, gid)
      pes <- groupElementsAs(pi, pid, Some(gid)) // parent elements
      ces <- groupElementsAs(pi, pid, Some(clo)) // clone elements
    yield pes.corresponds(ces):
      case (Left(_), Left(_))   => true
      case (Right(_), Right(_)) => true
      case _                    => false
  }

  test("clone of nested group with things inside") {
    for
      pid <- createProgramAs(pi)
      x   <- createGroupAs(pi, pid) // top level group
      gid <- createGroupAs(pi, pid, Some(x)) // the group we're going to clone
      _   <- createObservationInGroupAs(pi, pid, Some(gid))
      _   <- createGroupAs(pi, pid, Some(gid))        
      clo <- cloneGroupAs(pi, gid)
      pes <- groupElementsAs(pi, pid, Some(gid)) // parent elements
      ces <- groupElementsAs(pi, pid, Some(clo)) // clone elements
    yield pes.corresponds(ces):
      case (Left(_), Left(_))   => true
      case (Right(_), Right(_)) => true
      case _                    => false
  }

  test("can't clone a system group") {

    val setup =
      for
        pid <- createProgramAs(pi)
        gid <- createGroupAs(pi, pid)
        _   <- updateGroupSystem(gid, true)
      yield gid

    setup.flatMap: gid =>
      expectOdbError(
        user = pi,
        query = s"""
          mutation {
            cloneGroup(input: {
              groupId: "$gid",
              SET: {
                parentGroupIndex: 2
              }
            }) {
              originalGroup {
                parentIndex
              }
              newGroup {
                parentIndex
              }
            }
          }
        """,
        expected = {
          case OdbError.UpdateFailed(Some("System groups cannot be cloned.")) => // ok
        }  
      )
        
  }


}
