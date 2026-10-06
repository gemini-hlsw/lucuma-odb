// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.literal.*
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class cloneGroup extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2 = TestUsers.Standard.pi(nextId, nextId)
  val guest = TestUsers.guest(nextId)
  val validUsers: List[User] = List(pi, pi2, guest)

  private def cloneGroupQuery(gid: Group.Id): String =
    s"""
      mutation {
        cloneGroup(input: { groupId: "$gid" }) {
          newGroup { id }
        }
      }
    """

  test("a pi cannot clone a group in another pi's program"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- expectOdbError(
               user     = pi2,
               query    = cloneGroupQuery(gid),
               expected = { case OdbError.NotAuthorized(pi2.id, _) => }
             )
      ids <- groupElementsAs(pi, pid, None)
    yield assertEquals(ids, List(Left(gid)))

  test("a guest cannot clone a group in another user's program"):
    for
      pid <- createProgramAs(pi)
      gid <- createGroupAs(pi, pid)
      _   <- expectOdbError(
               user     = guest,
               query    = cloneGroupQuery(gid),
               expected = { case OdbError.NotAuthorized(guest.id, _) => }
             )
    yield ()

  test("can't clone into a group in another program"):
    for
      pid1 <- createProgramAs(pi)
      gid1 <- createGroupAs(pi, pid1)
      pid2 <- createProgramAs(pi)
      gid2 <- createGroupAs(pi, pid2)
      _    <- expectOdbError(
                user = pi,
                query = s"""
                  mutation {
                    cloneGroup(input: {
                      groupId: "$gid1",
                      SET: {
                        parentGroup: "$gid2"
                      }
                    }) {
                      newGroup { id }
                    }
                  }
                """,
                expected = {
                  case OdbError.InvalidArgument(Some(s"Group $gid2 is not in program $pid1.")) => ()
                }
              )
      ids1 <- groupElementsAs(pi, pid1, None)
      ids2 <- groupElementsAs(pi, pid2, Some(gid2))
    yield
      assertEquals(ids1, List(Left(gid1)))
      assertEquals(ids2, Nil)

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
                newGroup {
                  parentIndex
                }
              }
            }
          """,
          expected = Right(json"""
            {
              "cloneGroup" : {
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
