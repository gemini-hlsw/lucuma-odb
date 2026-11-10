// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.AllocationInput

// NGO users may read, but not edit, programs in which their partner holds an
// allocation (unless the proposal was not accepted) or, while the proposal is
// undecided, requests time (partner splits, or the PI's partner in a Fast
// Turnaround or Poor Weather proposal).
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

  // The DD allocation is a time accounting category that is not a partner, so
  // it must be ignored rather than decoded as one.
  def createProgramWithUsAllocation: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      _   <- setAllocationsAs(staff, pid, List(
               AllocationInput(TimeAccountingCategory.US, ScienceBand.Band1, 1.hourTimeSpan),
               AllocationInput(TimeAccountingCategory.DD, ScienceBand.Band1, 1.hourTimeSpan)
             ))
    yield pid

  def hasReadAccess(user: User, pid: Program.Id): IO[Boolean] =
    withServices(user): s =>
      s.session.transaction.use: xa =>
        s.programUserService.userHasReadAccess(pid)(using xa)

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

  test("partner splits stop counting once the proposal is deleted"):
    for
      pid <- createProgramWithUsSplit
      _   <- deleteProposal(staff, pid).map(assert(_))
      _   <- expectVisibility(pid, us = false, ca = false)
    yield ()

  // userHasReadAccess backs executionConfig and attachment downloads
  test("service-level read access follows the same rule"):
    for
      split <- createProgramWithUsSplit
      alloc <- createProgramWithUsAllocation
      none  <- createProgramAs(pi)
      rs    <- List(
                 hasReadAccess(ngoUs, split), hasReadAccess(ngoCa, split),
                 hasReadAccess(ngoUs, alloc), hasReadAccess(ngoCa, alloc),
                 hasReadAccess(ngoUs, none)
               ).sequence
    yield assertEquals(rs, List(true, false, true, false, false))

  // A submitted program requires a semester, which setProposalSubmittedDirectly supplies.
  def setStatus(pid: Program.Id, status: ProposalStatus): IO[Unit] =
    setProposalSubmittedDirectly(pid) *> setProposalStatusDirectly(pid, status)

  def undecided(status: ProposalStatus): Boolean =
    status === ProposalStatus.NotSubmitted || status === ProposalStatus.Submitted

  // A proposal with a CA PI to a call of the given type.
  def createProgramWithCaPi(callType: GeminiCallForProposalsType, callProps: String): IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff, callType)
      pid <- createProgramWithPiAffiliation(pi, PartnerLink.HasGeminiPartner(Partner.CA))
      _   <- addProposal(pi, pid, cid.some, callProps.some)
    yield pid

  test("a partner split counts only while the proposal is undecided"):
    ProposalStatus.values.toList.traverse_ : status =>
      for
        pid <- createProgramWithUsSplit
        _   <- setStatus(pid, status)
        _   <- expectVisibility(pid, us = undecided(status), ca = false)
      yield ()

  test("an allocation counts unless the proposal was not accepted"):
    ProposalStatus.values.toList.traverse_ : status =>
      for
        pid <- createProgramWithUsAllocation
        _   <- addProposal(pi, pid)
        _   <- setStatus(pid, status)
        _   <- expectVisibility(pid, us = status =!= ProposalStatus.NotAccepted, ca = false)
      yield ()

  test("the PI's partner sees an undecided Fast Turnaround or Poor Weather proposal"):
    val calls = List(
      (GeminiCallForProposalsType.FastTurnaround, "fastTurnaround: {}"),
      (GeminiCallForProposalsType.PoorWeather,    "poorWeather: {}")
    )
    (calls, ProposalStatus.values.toList).tupled.traverse_ : (call, status) =>
      for
        pid <- createProgramWithCaPi(call._1, call._2)
        _   <- setStatus(pid, status)
        _   <- expectVisibility(pid, us = false, ca = undecided(status))
      yield ()

  test("the PI's partner does not see other proposals without a split"):
    val props = "queue: { considerForBand3: DO_NOT_CONSIDER, partnerSplits: [{ partner: US, percent: 100 }] }"
    createProgramWithCaPi(GeminiCallForProposalsType.RegularSemester, props).flatMap: pid =>
      expectVisibility(pid, us = true, ca = false)

  test("service-level read access follows the proposal status"):
    for
      ft <- createProgramWithCaPi(GeminiCallForProposalsType.FastTurnaround, "fastTurnaround: {}")
      a  <- hasReadAccess(ngoCa, ft)
      _  <- setStatus(ft, ProposalStatus.NotAccepted)
      b  <- hasReadAccess(ngoCa, ft)
    yield assertEquals((a, b), (true, false))

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

  test("an NGO is notified of edits to a program its partner holds an allocation in"):
    createProgramWithUsAllocation.flatMap: pid =>
      expectProgramEditEvents(ngoUs, pid, List(
        json"""{ "programEdit": { "editType": "UPDATED", "value": { "name": "renamed" } } }"""
      ))

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

  test("an NGO cannot edit a note in a program it can see"):
    for
      pid <- createProgramWithUsSplit
      nid <- createProgramNoteAs(pi, pid, "Note", "text".some)
      _   <- expect(
               user  = ngoUs,
               query = s"""
                 mutation {
                   updateProgramNotes(input: {
                     WHERE: { id: { EQ: "$nid" } }
                     SET: { title: "ngo edit" }
                   }) {
                     programNotes { id }
                   }
                 }
               """,
               expected = Right(json"""{ "updateProgramNotes": { "programNotes": [] } }""")
             )
    yield ()

  test("an NGO cannot clone a target in a program it can see"):
    for
      pid <- createProgramWithUsSplit
      tid <- createTargetAs(pi, pid)
      _   <- interceptOdbError(
               query(ngoUs, s"""
                 mutation {
                   cloneTarget(input: { targetId: "$tid" }) {
                     newTarget { id }
                   }
                 }
               """)
             ):
               case OdbError.InvalidTarget(`tid`, _) => ()
    yield ()

}
