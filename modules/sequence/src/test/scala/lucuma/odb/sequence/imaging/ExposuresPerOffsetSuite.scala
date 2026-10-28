// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class ExposuresPerOffsetSuite extends ScalaCheckSuite:

  import ExposuresPerOffset.*

  private def p(n: Int): PosInt = PosInt.unsafeFrom(n)

  private val genK: Gen[PosInt] =
    Gen.choose(1, 10).map(p)

  private val genCounts: Gen[List[(Int, PosInt)]] =
    for
      n  <- Gen.choose(1, 4)
      cs <- Gen.listOfN(n, Gen.choose(1, 60).map(p))
    yield cs.zipWithIndex.map(_.swap)

  test("stopSizes"):
    assertEquals(stopSizes(p(6), p(3)), List(3, 3))
    assertEquals(stopSizes(p(7), p(3)), List(3, 3, 1))
    assertEquals(stopSizes(p(2), p(3)), List(2))
    assertEquals(stopSizes(p(4), p(1)), List(1, 1, 1, 1))

  test("stopSizes, exposures per offset far exceeding the count"):
    assertEquals(stopSizes(p(2), PosInt.MaxValue), List(2))
    assertEquals(stopSizes(PosInt.MaxValue, PosInt.MaxValue), List(Int.MaxValue))

  property("stopSizes honors the count, with only the last stop short"):
    forAll(Gen.choose(1, 100).map(p), genK) { (count, k) =>
      val sizes = stopSizes(count, k)
      assertEquals(sizes.sum, count.value)
      assert(sizes.init.forall(_ == k.value))
      assert(sizes.last >= 1 && sizes.last <= k.value)
    }

  test("repeat"):
    assertEquals(repeat(List("a", "b", "c"), p(2), p(5)), List("a", "a", "b", "b", "c"))
    assertEquals(repeat(List("a", "b"), p(3), p(2)), List("a", "a"))

  test("repeat, exposures per offset far exceeding the count"):
    assertEquals(repeat(List("a"), PosInt.MaxValue, p(3)), List("a", "a", "a"))

  test("offset counts, exposures per offset far exceeding the count"):
    assertEquals(groupedOffsetCount(NonEmptyList.of(p(2), p(4)), PosInt.MaxValue), 1)
    assertEquals(interleavedOffsetCount(NonEmptyList.of(p(2), p(4)), PosInt.MaxValue), 2)

  test("interleave, science example"):
    // N(A) = 2, N(B) = 4, n = 3
    assertEquals(
      interleave(List("A" -> p(2), "B" -> p(4)), p(3)),
      List(List("A" -> 2, "B" -> 3, "B" -> 1))
    )

  test("interleave, exposures per offset far exceeding the count"):
    assertEquals(
      interleave(List("A" -> p(2), "B" -> p(4)), PosInt.MaxValue),
      List(List("A" -> 2, "B" -> 4))
    )

  test("interleave, one exposure per offset"):
    // The behavior that predates exposuresPerOffset: as many groups as the
    // smallest count, with the remainder spread over the first groups.
    assertEquals(
      interleave(List("A" -> p(2), "B" -> p(5)), p(1)),
      List(
        List("A" -> 1, "B" -> 1, "B" -> 1, "B" -> 1),
        List("A" -> 1, "B" -> 1, "B" -> 1)
      )
    )

  property("interleave preserves each filter's stops, in order"):
    forAll(genCounts, genK) { (counts, k) =>
      val plan = interleave(counts, k)
      counts.foreach: (f, count) =>
        assertEquals(plan.flatten.collect { case (`f`, size) => size }, stopSizes(count, k))
    }

  property("interleave has one group per stop of the filter with the fewest"):
    forAll(genCounts, genK) { (counts, k) =>
      val plan = interleave(counts, k)
      assertEquals(plan.size, counts.map((_, c) => stopCount(c, k)).min)
      plan.foreach: group =>
        assertEquals(group.map(_._1).distinct, counts.map(_._1))
    }
