// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

class programUserAccess extends OdbSuite:

  val pi:       User = TestUsers.Standard.pi(1, 101)
  val coi:      User = TestUsers.Standard.pi(2, 102)
  val coiRo:    User = TestUsers.Standard.pi(3, 103)
  val external: User = TestUsers.Standard.pi(4, 104)

  override val validUsers: List[User] = List(pi, coi, coiRo, external)

  def setup: IO[(Program.Id, Observation.Id)] =
    for
      _   <- createUsers(pi, coi, coiRo, external)
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- List(
               coi      -> ProgramUserRole.Coi,
               coiRo    -> ProgramUserRole.CoiRO,
               external -> ProgramUserRole.External
             ).traverse_ { (u, r) =>
               addProgramUserAs(pi, pid, r).flatMap(m => linkUserAs(pi, m, u.id))
             }
    yield (pid, oid)

  def updateObservation(user: User, oid: Observation.Id, expected: List[String]): IO[Unit] =
    expect(
      user     = user,
      query    = s"""
        mutation {
          updateObservations(input: {
            SET: { subtitle: "Foo" },
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { subtitle }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": ${expected.map(s => Json.obj("subtitle" -> s.asJson))}
          }
        }
      """.asRight
    )

  def updateProgram(user: User, pid: Program.Id, expected: List[String]): IO[Unit] =
    expect(
      user     = user,
      query    = s"""
        mutation {
          updatePrograms(input: {
            SET: { name: "Bar" },
            WHERE: { id: { EQ: ${pid.asJson} } }
          }) {
            programs { name }
          }
        }
      """,
      expected = json"""
        {
          "updatePrograms": {
            "programs": ${expected.map(s => Json.obj("name" -> s.asJson))}
          }
        }
      """.asRight
    )

  def readProgram(user: User, pid: Program.Id, visible: Boolean): IO[Unit] =
    expect(
      user     = user,
      query    = s"""
        query { program(programId: ${pid.asJson}) { id } }
      """,
      expected = json"""
        {
          "program": ${Option.when(visible)(Json.obj("id" -> pid.asJson))}
        }
      """.asRight
    )

  test("COI may update"):
    setup.flatMap: (pid, oid) =>
      updateObservation(coi, oid, List("Foo")) >>
      updateProgram(coi, pid, List("Bar"))

  test("COI_RO may not update"):
    setup.flatMap: (pid, oid) =>
      updateObservation(coiRo, oid, Nil) >>
      updateProgram(coiRo, pid, Nil)

  test("EXTERNAL may not update"):
    setup.flatMap: (pid, oid) =>
      updateObservation(external, oid, Nil) >>
      updateProgram(external, pid, Nil)

  test("COI_RO and EXTERNAL may read"):
    setup.flatMap: (pid, _) =>
      readProgram(coiRo, pid, visible = true) >>
      readProgram(external, pid, visible = true)
