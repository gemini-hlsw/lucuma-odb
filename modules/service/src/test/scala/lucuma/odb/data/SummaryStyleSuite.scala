// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.all.*
import lucuma.core.enums.Observatory
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceSubtype
import munit.FunSuite

class SummaryStyleSuite extends FunSuite:

  import ScienceSubtype.*
  import SummaryStyle.*

  private def gemini(subtype: ScienceSubtype, partner: Option[Partner]): SummaryStyle =
    SummaryStyle.forProposal(subtype.some, Observatory.Gemini, partner)

  test("the proposal type decides on its own, whatever the partner"):
    Partner.values.map(_.some).toList.appended(None).foreach: p =>
      assertEquals(gemini(LargeProgram, p),       GeminiDarp)
      assertEquals(gemini(DemoScience, p),        GeminiInvestigatorsAtEnd)
      assertEquals(gemini(SystemVerification, p), GeminiInvestigatorsAtEnd)
      assertEquals(gemini(FastTurnaround, p),     GeminiNoInvestigators)
      assertEquals(gemini(DirectorsTime, p),      GeminiStandard)
      assertEquals(gemini(PoorWeather, p),        GeminiStandard)

  test("queue and classical proposals follow the partner"):
    val expected = List(
      Partner.AR.some -> GeminiDarp,
      Partner.BR.some -> GeminiDarp,
      Partner.CA.some -> GeminiDarp,
      Partner.CL.some -> Chile,
      Partner.KR.some -> GeminiDarp,
      Partner.UH.some -> GeminiStandard,
      Partner.US.some -> NoirlabDarp,
      none            -> GeminiStandard
    )
    expected.foreach: (p, style) =>
      assertEquals(gemini(Queue, p),     style, clue = p)
      assertEquals(gemini(Classical, p), style, clue = p)

  test("exchange proposals go by the observatory"):
    assertEquals(SummaryStyle.forProposal(none, Observatory.Subaru, none), GeminiDarp)
    assertEquals(SummaryStyle.forProposal(none, Observatory.Keck, none),   GeminiStandard)

  test("a program without a proposal type gets the default"):
    assertEquals(SummaryStyle.forProposal(none, Observatory.Gemini, none), SummaryStyle.Default)
