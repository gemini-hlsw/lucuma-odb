// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.TimeAccountingCategory.*
import lucuma.core.model.Program
import lucuma.odb.service.TacFeedbackService.FeedbackContext
import lucuma.odb.service.TacFeedbackService.eligibility

class TacFeedbackEligibilitySuite extends munit.FunSuite:

  val pid: Program.Id = Program.Id.fromLong(1L).get

  // A submitted proposal with nothing else that would make a category eligible.
  val empty: FeedbackContext =
    FeedbackContext(ProposalStatus.Submitted, none, none, none, Set.empty)

  def canSet(category: TimeAccountingCategory, ctx: FeedbackContext): Boolean =
    eligibility(pid, category, ctx, forDelete = false).isRight

  def canDelete(category: TimeAccountingCategory, ctx: FeedbackContext): Boolean =
    eligibility(pid, category, ctx, forDelete = true).isRight

  val partnerCategories: List[TimeAccountingCategory] =
    Partner.values.toList.map(_.timeAccountingCategory)

  test("feedback may only be set or deleted while the proposal is submitted"):
    for
      status   <- ProposalStatus.values.toList.filterNot(_ === ProposalStatus.Submitted)
      category <- TimeAccountingCategory.values.toList
    do
      val ctx = FeedbackContext(status, GeminiCallForProposalsType.values.headOption, none, none, Partner.values.toSet)
      assert(!canSet(category, ctx), s"$category set in $status")
      assert(!canDelete(category, ctx), s"$category deleted in $status")

  test("deletion requires only a submitted proposal"):
    TimeAccountingCategory.values.toList.foreach: category =>
      assert(canDelete(category, empty), category.toString)

  test("a partner requires a non-zero split"):
    Partner.values.toList.foreach: p =>
      val c = p.timeAccountingCategory
      assert(!canSet(c, empty), p.toString)
      assert(canSet(c, empty.copy(splitPartners = Set(p))), p.toString)
      assert(!canSet(c, empty.copy(splitPartners = Partner.values.toSet - p)), p.toString)

  test("a partner without a split requires a Fast Turnaround or Poor Weather proposal with a PI affiliated with it"):
    val piPartnerCalls = Set(GeminiCallForProposalsType.FastTurnaround, GeminiCallForProposalsType.PoorWeather)
    for
      p <- Partner.values.toList
      t <- GeminiCallForProposalsType.values.toList.map(_.some) :+ none
    do
      val c   = p.timeAccountingCategory
      val ctx = empty.copy(cfpType = t)
      assertEquals(canSet(c, ctx.copy(piPartner = p.some)), t.exists(piPartnerCalls.contains), s"$p, $t")
      assert(!canSet(c, ctx.copy(piPartner = Partner.values.find(_ =!= p))), s"$p, $t, another PI partner")

  test("CAL, ENG and CFHT can never set feedback"):
    val everything = FeedbackContext(ProposalStatus.Submitted, none, Partner.US.some, ExchangePartner.Subaru.some, Partner.values.toSet)
    List(CAL, ENG, CFHT).foreach: c =>
      assertEquals(eligibility(pid, c, everything, forDelete = false), s"Time accounting category ${c.tag.toUpperCase} cannot provide TAC feedback.".asLeft)
      GeminiCallForProposalsType.values.foreach: t =>
        assert(!canSet(c, everything.copy(cfpType = t.some)), s"$c with $t")

  test("GT and LTP can set feedback for any proposal"):
    List(GT, LTP).foreach: c =>
      assert(canSet(c, empty), c.toString)

  test("JP and KECK require a proposal on behalf of their community"):
    assert(canSet(JP, empty.copy(exchangePartner = ExchangePartner.Subaru.some)))
    assert(canSet(KECK, empty.copy(exchangePartner = ExchangePartner.Keck.some)))
    assert(!canSet(JP, empty.copy(exchangePartner = ExchangePartner.Keck.some)))
    assert(!canSet(KECK, empty.copy(exchangePartner = ExchangePartner.Subaru.some)))
    assert(!canSet(JP, empty))
    assert(!canSet(KECK, empty))

  test("DD, DS, LP and SV require a call for proposals of the matching type"):
    val matching = Map(
      DD -> GeminiCallForProposalsType.DirectorsTime,
      DS -> GeminiCallForProposalsType.DemoScience,
      LP -> GeminiCallForProposalsType.LargeProgram,
      SV -> GeminiCallForProposalsType.SystemVerification
    )
    matching.foreach: (c, required) =>
      assert(!canSet(c, empty), s"$c without a call")
      GeminiCallForProposalsType.values.foreach: t =>
        assertEquals(canSet(c, empty.copy(cfpType = t.some)), t === required, s"$c with $t")

  test("non-partner categories ignore splits and the PI's partner"):
    val partnered = empty.copy(piPartner = Partner.US.some, splitPartners = Partner.values.toSet)
    TimeAccountingCategory.values.toList.filterNot(partnerCategories.contains).filterNot(c => c === GT || c === LTP).foreach: c =>
      assert(!canSet(c, partnered), c.toString)
