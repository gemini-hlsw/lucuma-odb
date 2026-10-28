// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package imaging

import eu.timepit.refined.types.numeric.PosInt
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class ExposuresPerOffsetSuite extends ScalaCheckSuite:

  import ExposuresPerOffset.*

  private def k(n: Int): PosInt = PosInt.unsafeFrom(n)

  private val genK: Gen[PosInt] =
    Gen.choose(1, 10).map(k)

  private val genCounts: Gen[List[(Int, Int)]] =
    for
      n  <- Gen.choose(1, 4)
      cs <- Gen.listOfN(n, Gen.choose(1, 60))
    yield cs.zipWithIndex.map(_.swap)

  test("stopSizes"):
    assertEquals(stopSizes(6, k(3)), List(3, 3))
    assertEquals(stopSizes(7, k(3)), List(3, 3, 1))
    assertEquals(stopSizes(2, k(3)), List(2))
    assertEquals(stopSizes(4, k(1)), List(1, 1, 1, 1))

  property("stopSizes honors the count, with only the last stop short"):
    forAll(Gen.choose(1, 100), genK) { (count, k) =>
      val sizes = stopSizes(count, k)
      assertEquals(sizes.sum, count)
      assert(sizes.init.forall(_ == k.value))
      assert(sizes.last >= 1 && sizes.last <= k.value)
    }

  test("repeat"):
    assertEquals(repeat(List("a", "b", "c"), k(2), 5), List("a", "a", "b", "b", "c"))
    assertEquals(repeat(List("a", "b"), k(3), 2), List("a", "a"))

  test("interleave, science example"):
    // N(A) = 2, N(B) = 4, n = 3
    assertEquals(
      interleave(List("A" -> 2, "B" -> 4), k(3)),
      List(List("A" -> 2, "B" -> 3, "B" -> 1))
    )

  test("interleave, one exposure per offset"):
    // The behavior that predates exposuresPerOffset: as many groups as the
    // smallest count, with the remainder spread over the first groups.
    assertEquals(
      interleave(List("A" -> 2, "B" -> 5), k(1)),
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
