// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CallForProposalsType.RegularSemester
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ProgramUserRole.CoiRO
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class ShortCut_4460 extends OdbSuite with ObservingModeSetupOperations:

  val pi:         User = TestUsers.Standard.pi(1, 101)
  val staffCoiRo: User = TestUsers.Standard.staff(2, 102)
  val staff:      User = TestUsers.Standard.staff(3, 103)

  override val validUsers: List[User] =
    List(pi, staffCoiRo, staff)

  test("Staff as CoiRo can edit"):
    val setup: IO[Observation.Id] =
      for {
        _ <- createUsers(pi, staffCoiRo, staff)
        c <- createCallForProposalsAs(staff, RegularSemester)
        p <- createProgramWithNonPartnerPi(pi, "ShortCut 4460")
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
        _ <- setObservationWorkflowState(pi, o, ObservationWorkflowState.Inactive) // avoid submission check
        _ <- addProposal(pi, p, c.some)
        _ <- addPartnerSplits(pi, p)
        _ <- addCoisAs(pi, p)
        _ <- acceptProposal(staff, p)
        r <- addProgramUserAs(pi, p, CoiRO)
        _ <- linkUserAs(pi, r, staffCoiRo.id)
      } yield o

    def mutation(o: Observation.Id) = s"""
      mutation {
        updateObservations(input: {
          SET: {
            subtitle: "Foo"
          },
          WHERE: {
            id: { EQ: ${o.asJson} }
          }
        }) {
          observations {
            subtitle
          }
        }
      }
    """

    // Editing works for the COI-RO user because it is also a staff user.
    setup.flatMap: o =>
      expect(
        user     = staffCoiRo,
        query    = mutation(o),
        expected = json"""
          {
            "updateObservations": {
              "observations" : [
                {
                  "subtitle" : "Foo"
                }
              ]
            }
         }
        """.asRight
      )
