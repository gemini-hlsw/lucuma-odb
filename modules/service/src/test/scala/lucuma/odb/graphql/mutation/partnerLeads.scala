// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.odb.data.OdbError

class partnerLeads extends OdbSuite:

  val pi      = TestUsers.Standard.pi(nextId, nextId)
  val staff   = TestUsers.Standard.staff(nextId, nextId)
  val service = TestUsers.service(nextId)

  lazy val validUsers = List(pi, staff, service)

  import Partner.{AR, BR, CA, US}

  private def link(p: Partner): PartnerLink =
    PartnerLink.HasGeminiPartner(p)

  /** A lead argument for a split: omitted, null, or a program user. */
  enum Lead:
    case Omit
    case Reset
    case Choose(id: ProgramUser.Id)

  private def splitsArg(splits: List[(Partner, Int, Lead)]): String =
    splits.map: (p, pct, lead) =>
      val leadField = lead match
        case Lead.Omit       => ""
        case Lead.Reset      => "leadId: null"
        case Lead.Choose(id) => s"""leadId: "$id""""
      s"{ partner: ${p.tag.toScreamingSnakeCase}, percent: $pct, $leadField }"
    .mkString("[", ", ", "]")

  private def updateSplitsQuery(pid: Program.Id, splits: List[(Partner, Int, Lead)]): String =
    s"""
      mutation {
        updateProposal(input: {
          programId: "$pid"
          SET: { gemini: { queue: { partnerSplits: ${splitsArg(splits)} } } }
        }) {
          proposal { category }
        }
      }
    """

  private def setSplits(pid: Program.Id, splits: (Partner, Int, Lead)*): IO[Unit] =
    query(pi, updateSplitsQuery(pid, splits.toList)).void

  private def setSplitsExpectingError(pid: Program.Id, splits: (Partner, Int, Lead)*): IO[Unit] =
    expectOdbError(
      user     = pi,
      query    = updateSplitsQuery(pid, splits.toList),
      expected = { case OdbError.InvalidArgument(_) => }
    )

  /**
   * The partner leads read from the splits, checked against the users' own
   * `isPartnerLead` flags, which must name the same people.
   */
  private def leadsOf(pid: Program.Id): IO[Map[Partner, ProgramUser.Id]] =
    query(
      service,
      s"""
        query {
          program(programId: "$pid") {
            proposal {
              gemini {
                ... on Queue { partnerSplits { partner lead { id } } }
              }
            }
            pi    { id isPartnerLead }
            users { id isPartnerLead }
          }
        }
      """
    ).flatMap: js =>
      val prog   = js.hcursor.downField("program")
      val splits = prog.downFields("proposal", "gemini", "partnerSplits").values.toList.flatten
      val leads  = splits.flatMap: s =>
        val c = s.hcursor
        for
          p  <- c.downField("partner").as[String].toOption.flatMap(t => Enumerated[Partner].fromTag(t.toLowerCase))
          id <- c.downFields("lead", "id").as[ProgramUser.Id].toOption
        yield p -> id
      .toMap

      val users   = prog.downField("pi").focus.toList ::: prog.downField("users").values.toList.flatten
      val flagged = users.collect:
        case j if j.hcursor.downField("isPartnerLead").as[Boolean].contains(true) =>
          j.hcursor.downField("id").as[ProgramUser.Id].toOption.get
      .toSet

      IO(assertEquals(flagged, leads.values.toSet, "isPartnerLead disagrees with PartnerSplit.lead")).as(leads)

  private def assertLeads(pid: Program.Id, expected: (Partner, ProgramUser.Id)*): IO[Unit] =
    leadsOf(pid).map(assertEquals(_, expected.toMap))

  private def coi(pid: Program.Id, p: Partner): IO[ProgramUser.Id] =
    addProgramUserAs(pi, pid, ProgramUserRole.Coi, link(p))

  /** A queue proposal whose PI is affiliated with `piLink`. */
  private def proposal(piLink: PartnerLink): IO[(Program.Id, ProgramUser.Id)] =
    for
      pid  <- createProgramWithPiAffiliation(pi, piLink)
      _    <- addProposal(pi, pid)
      puid <- piProgramUserIdAs(pi, pid)
    yield (pid, puid)

  test("defaults: the PI for the PI's partner, the only candidate, otherwise none"):
    for
      (pid, piId) <- proposal(link(CA))
      ar          <- coi(pid, AR)
      _           <- coi(pid, BR)
      _           <- coi(pid, BR)
      _           <- setSplits(pid, (CA, 40, Lead.Omit), (AR, 30, Lead.Omit), (BR, 30, Lead.Omit))
      _           <- assertLeads(pid, CA -> piId, AR -> ar)
    yield ()

  test("an explicit choice is kept until reset"):
    for
      (pid, piId) <- proposal(link(CA))
      _           <- coi(pid, BR)
      br2         <- coi(pid, BR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (BR, 50, Lead.Choose(br2)))
      _           <- assertLeads(pid, CA -> piId, BR -> br2)
      _           <- setSplits(pid, (CA, 60, Lead.Omit), (BR, 40, Lead.Omit))
      _           <- assertLeads(pid, CA -> piId, BR -> br2)
      _           <- setSplits(pid, (CA, 60, Lead.Omit), (BR, 40, Lead.Reset))
      _           <- assertLeads(pid, CA -> piId)
    yield ()

  test("adding a second candidate keeps the default lead"):
    for
      (pid, piId) <- proposal(link(CA))
      ar          <- coi(pid, AR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (AR, 50, Lead.Omit))
      _           <- coi(pid, AR)
      _           <- assertLeads(pid, CA -> piId, AR -> ar)
    yield ()

  test("a lead must be eligible for the partner"):
    for
      (pid, _) <- proposal(link(CA))
      ar       <- coi(pid, AR)
      _        <- coi(pid, BR)
      _        <- setSplitsExpectingError(pid, (CA, 50, Lead.Omit), (BR, 50, Lead.Choose(ar)))
    yield ()

  test("only the PI may lead the PI's partner"):
    for
      (pid, _) <- proposal(link(CA))
      ca       <- coi(pid, CA)
      _        <- setSplitsExpectingError(pid, (CA, 100, Lead.Choose(ca)))
    yield ()

  test("a partner without requested time has no lead"):
    for
      (pid, piId) <- proposal(link(CA))
      ar          <- coi(pid, AR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (AR, 50, Lead.Omit))
      _           <- assertLeads(pid, CA -> piId, AR -> ar)
      _           <- setSplits(pid, (CA, 100, Lead.Omit), (AR, 0, Lead.Choose(ar)))
      _           <- assertLeads(pid, CA -> piId)
    yield ()

  test("deleting the lead falls back to the default"):
    for
      (pid, piId) <- proposal(link(CA))
      br1         <- coi(pid, BR)
      br2         <- coi(pid, BR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (BR, 50, Lead.Choose(br2)))
      _           <- deleteProgramUserAs(pi, br2)
      _           <- assertLeads(pid, CA -> piId, BR -> br1)
    yield ()

  test("a lead whose affiliation changes no longer leads the old partner"):
    for
      (pid, piId) <- proposal(link(CA))
      ar          <- coi(pid, AR)
      br          <- coi(pid, BR)
      _           <- setSplits(pid, (CA, 40, Lead.Omit), (AR, 30, Lead.Omit), (BR, 30, Lead.Omit))
      _           <- assertLeads(pid, CA -> piId, AR -> ar, BR -> br)
      _           <- updateProgramUserAs(pi, ar, link(BR))
      _           <- assertLeads(pid, CA -> piId, BR -> br)
    yield ()

  test("a lead who stops being an investigator is replaced"):
    for
      (pid, piId) <- proposal(link(CA))
      ar1         <- coi(pid, AR)
      ar2         <- coi(pid, AR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (AR, 50, Lead.Choose(ar1)))
      _           <- query(
                       pi,
                       s"""
                         mutation {
                           changeProgramUserRole(input: { programUserId: "$ar1", newRole: COI_RO }) {
                             programUser { role }
                           }
                         }
                       """
                     )
      _           <- assertLeads(pid, CA -> piId, AR -> ar1) // still an investigator
      _           <- query(
                       staff,
                       s"""
                         mutation {
                           changeProgramUserRole(input: { programUserId: "$ar1", newRole: EXTERNAL }) {
                             programUser { role }
                           }
                         }
                       """
                     )
      _           <- assertLeads(pid, CA -> piId, AR -> ar2)
    yield ()

  test("a new PI leads their own partner"):
    for
      (pid, piId) <- proposal(link(CA))
      ar1         <- coi(pid, AR)
      ar2         <- coi(pid, AR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (AR, 50, Lead.Choose(ar1)))
      _           <- query(
                       pi,
                       s"""
                         mutation {
                           changePrincipalInvestigator(input: { programUserId: "$ar2" }) {
                             programUser { id }
                           }
                         }
                       """
                     )
      // The old PI, now a co-investigator, remains the only CA candidate.
      _           <- assertLeads(pid, CA -> piId, AR -> ar2)
    yield ()

  test("a non-partner PI leads US"):
    for
      (pid, piId) <- proposal(PartnerLink.HasNonPartner)
      us          <- coi(pid, US)
      _           <- setSplits(pid, (US, 100, Lead.Omit))
      _           <- assertLeads(pid, US -> piId)
      _           <- setSplitsExpectingError(pid, (US, 100, Lead.Choose(us)))
    yield ()

  test("a non-partner co-investigator leads nothing"):
    for
      (pid, _) <- proposal(link(CA))
      _        <- addProgramUserAs(pi, pid, ProgramUserRole.Coi, PartnerLink.HasNonPartner)
      _        <- setSplits(pid, (CA, 0, Lead.Omit), (US, 100, Lead.Omit))
      _        <- assertLeads(pid)
    yield ()

  test("a demoted non-partner PI no longer leads US"):
    for
      (pid, piId) <- proposal(PartnerLink.HasNonPartner)
      us          <- coi(pid, US)
      _           <- setSplits(pid, (US, 100, Lead.Omit))
      _           <- assertLeads(pid, US -> piId)
      _           <- query(
                       pi,
                       s"""
                         mutation {
                           changePrincipalInvestigator(input: { programUserId: "$us" }) {
                             programUser { id }
                           }
                         }
                       """
                     )
      _           <- assertLeads(pid, US -> us)
    yield ()

  test("updating users across programs syncs each program"):
    for
      (pid1, pi1) <- proposal(link(CA))
      (pid2, pi2) <- proposal(link(CA))
      _           <- setSplits(pid1, (CA, 50, Lead.Omit), (AR, 50, Lead.Omit))
      _           <- setSplits(pid2, (CA, 50, Lead.Omit), (AR, 50, Lead.Omit))
      _           <- assertLeads(pid1, CA -> pi1)
      _           <- assertLeads(pid2, CA -> pi2)
      a1          <- coi(pid1, BR)
      a2          <- coi(pid2, BR)
      _           <- query(
                       pi,
                       s"""
                         mutation {
                           updateProgramUsers(input: {
                             SET: { partnerLink: { geminiPartner: AR } }
                             WHERE: { id: { IN: ["$a1", "$a2"] } }
                           }) {
                             programUsers { id }
                           }
                         }
                       """
                     )
      _           <- assertLeads(pid1, CA -> pi1, AR -> a1)
      _           <- assertLeads(pid2, CA -> pi2, AR -> a2)
    yield ()

  test("deleting the proposal clears the leads"):
    for
      (pid, piId) <- proposal(link(CA))
      ar          <- coi(pid, AR)
      _           <- setSplits(pid, (CA, 50, Lead.Omit), (AR, 50, Lead.Omit))
      _           <- assertLeads(pid, CA -> piId, AR -> ar)
      _           <- deleteProposal(staff, pid)
      users       <- query(service, s"""query { program(programId: "$pid") { pi { isPartnerLead } users { isPartnerLead } } }""")
    yield
      val flags = users.hcursor.downFields("program", "pi", "isPartnerLead").as[Boolean].toOption.toList :::
        users.hcursor.downFields("program", "users").values.toList.flatten.flatMap(_.hcursor.downField("isPartnerLead").as[Boolean].toOption)
      assertEquals(flags.filter(identity), List.empty[Boolean])

end partnerLeads
