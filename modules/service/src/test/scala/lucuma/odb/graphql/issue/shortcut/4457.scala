// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CallForProposalsType.RegularSemester
import lucuma.core.enums.ProgramUserRole.CoiRO
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class ShortCut_4457 extends OdbSuite with ObservingModeSetupOperations:

  val pi:    User = TestUsers.Standard.pi(1, 101)
  val coiRo: User = TestUsers.Standard.pi(2, 102)
  val staff: User = TestUsers.Standard.staff(3, 103)
  val serviceUser = TestUsers.service(4)

  override val validUsers: List[User] =
    List(pi, coiRo, staff)

  val setup: IO[(Program.Id, Observation.Id)] =
    for
      _ <- createUsers(pi, coiRo, staff)
      c <- createCallForProposalsAs(staff, RegularSemester)
      p <- createProgramWithNonPartnerPi(pi, "ShortCut 4457")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- computeItcResultAs(pi, o)
      _ <- addProposal(pi, p, c.some)
      _ <- addPartnerSplits(pi, p)
      _ <- addCoisAs(pi, p)
      _ <- acceptProposal(staff, p)
      m <- addProgramUserAs(pi, p, CoiRO)
      _ <- linkUserAs(pi, m, coiRo.id)
      _ <- runObscalcUpdateAs(serviceUser, p, o)
    yield (p, o)

  test("Read-only Co-Investigators can access accepted program"):

    def query(o: Observation.Id) = s"""
      query {
        observation(observationId: "$o") {
          index
        }
      }
    """

    val expected: Json =
      json"""
        {
          "observation": {
            "index": 1
          }
        }
      """

    // access works for the COI-RO user after proposal acceptance
    setup.flatMap: (_, o) =>
      expect(
        user     = coiRo,
        query    = query(o),
        expected = expected.asRight
      )

  test("Read-only Co-Investigators can access accepted program, even if workflow is selected"):

    def query(o: Observation.Id) = s"""
      query {
        observation(observationId: "$o") {
          index
          workflow { value { state } }
        }
      }
    """

    val expected: Json =
      json"""
        {
          "observation": {
            "index": 1,
            "workflow": { "value" : { "state": "UNAPPROVED" } }
          }
        }
      """

    // access works for the COI-RO user after proposal acceptance
    setup.flatMap: (_, o) =>
      expect(
        user     = coiRo,
        query    = query(o),
        expected = expected.asRight
      )

  test("Read-only Co-Investigators can access accepted program, even if workflow is selected in an observations query"):

    def query(p: Program.Id) = s"""
      query {
        program(programId: "$p") {
          observations {
            matches {
              index
              workflow { value { state } }
            }
          }
        }
      }
    """

    val expected: Json =
      json"""
        {
          "program": {
            "observations": {
              "matches": [
                {
                  "index": 1,
                  "workflow": { "value": { "state": "UNAPPROVED" } }
                }
              ]
            }
          }
        }
      """

    // access works for the COI-RO user after proposal acceptance
    setup.flatMap: (p, _) =>
      expect(
        user     = coiRo,
        query    = query(p),
        expected = expected.asRight
      )
