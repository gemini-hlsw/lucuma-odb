// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.option.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.core.util.TimeSpan
import munit.FunSuite

class ProposalServiceSuite extends FunSuite {

  private def hours(h: Double): TimeSpan =
    TimeSpan.unsafeFromMicroseconds((h * 3_600_000_000L).toLong)

  private def time(h: Double): CategorizedTime =
    CategorizedTime(ChargeClass.Program -> hours(h))

  private def range(
    min:   Double,
    max:   Double,
    state: CalculationState = CalculationState.Ready
  ): Option[CalculatedValue[CategorizedTimeRange]] =
    CalculatedValue(state, CategorizedTimeRange.from(time(min), time(max))).some

  test("no estimate at all"):
    assertEquals(ProposalService.timeRequestedText(none), "Not available")

  test("a zero estimate"):
    assertEquals(ProposalService.timeRequestedText(range(0.0, 0.0)), "Not available")

  test("equal minimum and maximum"):
    assertEquals(ProposalService.timeRequestedText(range(12.5, 12.5)), "12.50 hours")

  test("differing minimum and maximum"):
    assertEquals(ProposalService.timeRequestedText(range(10.0, 12.5)), "10.00 - 12.50 hours")

  test("rounds to two decimal places"):
    assertEquals(ProposalService.timeRequestedText(range(1.0 / 3.0, 1.0 / 3.0)), "0.33 hours")

  test("a stale estimate is reported like any other"):
    CalculationState.values.foreach: s =>
      assertEquals(ProposalService.timeRequestedText(range(12.5, 12.5, s)), "12.50 hours")

  test("html escaping"):
    assertEquals(ProposalService.escapeHtml("Ann & Bob"), "Ann &amp; Bob")
    assertEquals(ProposalService.escapeHtml("<script>x</script>"), "&lt;script&gt;x&lt;/script&gt;")
    assertEquals(ProposalService.escapeHtml("nothing to do"), "nothing to do")

}
