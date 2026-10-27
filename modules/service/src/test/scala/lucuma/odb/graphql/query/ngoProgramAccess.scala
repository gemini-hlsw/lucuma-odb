// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.input.AllocationInput

// NGO users may read, but not edit, programs in which their partner requests
// time (partner splits) or holds an allocation.
class ngoProgramAccess extends OdbSuite {

  val pi     = TestUsers.Standard.pi(nextId, nextId)
  val ngoUs  = TestUsers.Standard.ngo(nextId, nextId, Partner.US)
  val ngoCa  = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff  = TestUsers.Standard.staff(nextId, nextId)

  val validUsers = List(pi, ngoUs, ngoCa, staff)

  // Explore stores a split for every partner, so CA's 0% row must not count.
  def createProgramWithUsSplit: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100), (Partner.CA, 0)))
    yield pid

  def createProgramWithUsAllocation: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- setAllocationsAs(staff, pid, List(AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 1.hourTimeSpan)))
    yield pid

  def expectProgram(user: User, pid: Program.Id, visible: Boolean): IO[Unit] =
    expect(
      user  = user,
      query = s"""
        query {
          program(programId: "$pid") { id }
        }
      """,
      expected = Right(
        Json.obj("program" -> (if visible then Json.obj("id" -> pid.asJson) else Json.Null))
      )
    )

  def expectPrograms(user: User, pid: Program.Id, visible: Boolean): IO[Unit] =
    expect(
      user  = user,
      query = s"""
        query {
          programs(WHERE: { id: { EQ: "$pid" } }) {
            matches { id }
          }
        }
      """,
      expected = Right(
        Json.obj("programs" -> Json.obj("matches" ->
          (if visible then List(Json.obj("id" -> pid.asJson)) else Nil).asJson
        ))
      )
    )

  def expectVisibility(pid: Program.Id, us: Boolean, ca: Boolean): IO[Unit] =
    expectProgram(ngoUs, pid, us)  *>
    expectPrograms(ngoUs, pid, us) *>
    expectProgram(ngoCa, pid, ca)  *>
    expectPrograms(ngoCa, pid, ca)

  test("a non-zero partner split makes a program visible to that partner's NGO only"):
    createProgramWithUsSplit.flatMap(expectVisibility(_, us = true, ca = false))

  test("an allocation makes a program visible to that partner's NGO only"):
    createProgramWithUsAllocation.flatMap(expectVisibility(_, us = true, ca = false))

  test("a program with neither splits nor allocations is visible to no NGO"):
    createProgramAs(pi).flatMap(expectVisibility(_, us = false, ca = false))

  test("a program's observations are visible to the NGO"):
    for
      pid <- createProgramWithUsSplit
      oid <- createObservationAs(pi, pid)
      _   <- expect(
               user  = ngoUs,
               query = s"""
                 query {
                   observation(observationId: "$oid") { id }
                 }
               """,
               expected = Right(json"""{ "observation": { "id": $oid } }""")
             )
      _   <- expect(
               user  = ngoCa,
               query = s"""
                 query {
                   observation(observationId: "$oid") { id }
                 }
               """,
               expected = Right(json"""{ "observation": null }""")
             )
    yield ()

  def expectProgramEditEvents(user: User, pid: Program.Id, expected: List[Json]): IO[Unit] =
    subscriptionExpect(
      user      = user,
      query     = s"""
        subscription {
          programEdit(input: { programId: "$pid" }) {
            editType
            value { name }
          }
        }
      """,
      mutations = Right(
        query(pi, s"""
          mutation {
            updatePrograms(input: {
              WHERE: { id: { EQ: "$pid" } }
              SET: { name: "renamed" }
            }) {
              programs { id }
            }
          }
        """)
      ),
      expected  = expected
    )

  test("an NGO is notified of edits to a program it can see"):
    createProgramWithUsSplit.flatMap: pid =>
      expectProgramEditEvents(ngoUs, pid, List(
        json"""{ "programEdit": { "editType": "UPDATED", "value": { "name": "renamed" } } }"""
      ))

  test("an NGO is not notified of edits to a program it cannot see"):
    createProgramWithUsSplit.flatMap: pid =>
      expectProgramEditEvents(ngoCa, pid, Nil)

  test("an NGO cannot edit a program it can see"):
    createProgramWithUsSplit.flatMap: pid =>
      expect(
        user  = ngoUs,
        query = s"""
          mutation {
            updatePrograms(input: {
              WHERE: { id: { EQ: "$pid" } }
              SET: { name: "ngo edit" }
            }) {
              programs { id }
            }
          }
        """,
        expected = Right(json"""{ "updatePrograms": { "programs": [] } }""")
      )

  test("an NGO cannot edit an observation it can see"):
    for
      pid <- createProgramWithUsSplit
      oid <- createObservationAs(pi, pid)
      _   <- expect(
               user  = ngoUs,
               query = s"""
                 mutation {
                   updateObservations(input: {
                     WHERE: { id: { EQ: "$oid" } }
                     SET: { subtitle: "ngo edit" }
                   }) {
                     observations { id }
                   }
                 }
               """,
               expected = Right(json"""{ "updateObservations": { "observations": [] } }""")
             )
    yield ()

}
