// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.User

// https://app.shortcut.com/lucuma/story/7782
class ShortCut_7782 extends OdbSuite with DatabaseOperations:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  val staff = TestUsers.Standard.staff(nextId, nextId)
  val service = TestUsers.service(nextId)
  val validUsers = List(pi, staff, service)

  def setFlamingos2DisperserAs(user: User, oid: Observation.Id, disp: String) =
    query(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              SET: {
                observingMode: {
                  flamingos2LongSlit: {
                    disperser: $disp 
                  }
                }
              }
              WHERE: {
                id: {
                  EQ: "$oid"
                }
              }
            }
          ) {
            observations {
              id
            }
          }
        }
      """
    )

  test("submit a propodal with two F2 obs with different dispersers"):
    for
      cfp <- createCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, Some(cfp), None)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      tid <- createTargetAs(pi, pid)
      o1  <- createFlamingos2LongSlitObservationAs(pi, pid, tid)  // R1200HK by default
      o2  <- createFlamingos2LongSlitObservationAs(pi, pid, tid)  // a second one, but we will change it
      _   <- setFlamingos2DisperserAs(pi, o2, "R1200_JH")
      _   <- List(o1, o2).traverse(runObscalcUpdateAs(service, pid, _))
      _   <- setProposalStatus(pi, pid, "SUBMITTED") // was failing
    yield ()

