// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Offset
import munit.Location
import munit.ScalaCheckSuite

class RemainingSuite extends ScalaCheckSuite:

  val q0 = Offset.Q.Zero
  val q1 = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("1.0"))
  val q2 = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("2.0"))
  val q3 = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("3.0"))

  val zero  = NonNegInt.unsafeFrom(0)
  val one   = NonNegInt.unsafeFrom(1)
  val two   = NonNegInt.unsafeFrom(2)
  val three = NonNegInt.unsafeFrom(3)

  test("empty"):
    assert(Remaining.empty[Offset.Q].isEmpty)

  test("empty.take(0)"):
    assertEquals(Remaining.empty[Offset.Q].take(0)._1, Nil)

  test("empty.get"):
    assertEquals(Remaining.empty[Offset.Q].get(q0), None)

  val rem0 = Remaining(q0 -> 0, q1 -> 1, q2 -> 2, q3 -> 3)

  test("get"):
    assertEquals(rem0.get(q2).map(_.value), Some(2))

  test("not empty"):
    assertEquals(rem0.toMap, Map(q0 -> zero, q1 -> one, q2 -> two, q3 -> three))

  test("take(0) qs"):
    assertEquals(rem0.take(0)._1, Nil)

  test("take(0) remaining"):
    assertEquals(rem0.take(0)._2.toMap, Map(q0 -> zero, q1 -> one, q2 -> two, q3 -> three))

  test("take(1) increasing qs"):
    assertEquals(rem0.take(1)._1, List(q3))

  test("take(1) increasing remaining"):
    assertEquals(rem0.take(1)._2.toMap, Map(q0 -> zero, q1 -> one, q2 -> two, q3 -> two))

  test("take(2) increasing qs"):
    assertEquals(rem0.take(2)._1, List(q3, q2))

  test("take(2) increasing remaining"):
    println(rem0.take(2)._2.toMap)
    assertEquals(rem0.take(2)._2.toMap, Map(q0 -> zero, q1 -> one, q2 -> one, q3 -> two))

  test("take(3) increasing qs"):
    assertEquals(rem0.take(3)._1, List(q3, q3, q2))

  test("take(3) increasing remaining"):
    assertEquals(rem0.take(3)._2.toMap, Map(q0 -> zero, q1 -> one, q2 -> one, q3 -> one))

  test("take(4) increasing qs"):
    assertEquals(rem0.take(4)._1, List(q3, q3, q2, q1))

  test("take(4) increasing remaining"):
    assertEquals(rem0.take(4)._2.toMap, Map(q0 -> zero, q1 -> zero, q2 -> one, q3 -> one))

  test("take(5) increasing qs"):
    assertEquals(rem0.take(5)._1, List(q3, q3, q2, q2, q1))

  test("take(5) increasing remaining"):
    assertEquals(rem0.take(5)._2.toMap, Map(q0 -> zero, q1 -> zero, q2 -> zero, q3 -> one))

  test("take(6) increasing qs"):
    assertEquals(rem0.take(6)._1, List(q3, q3, q3, q2, q2, q1))

  test("take(6) increasing remaining"):
    assertEquals(rem0.take(6)._2.toMap, Map(q0 -> zero, q1 -> zero, q2 -> zero, q3 -> zero))

  test("take(7) increasing qs"):
    assertEquals(rem0.take(7)._1, List(q3, q3, q3, q2, q2, q1))

  test("take(7) increasing remaining"):
    assertEquals(rem0.take(7)._2.toMap, Map(q0 -> zero, q1 -> zero, q2 -> zero, q3 -> zero))

  test("take(6) increasing isEmpty"):
    assert(rem0.take(6)._2.isEmpty)

  test("take(2) tied"):
    assertEquals(Remaining(q0 -> 1, q1 -> 2, q2 -> 2, q3 -> 1).take(2)._1, List(q1, q2))

  val rem1 = Remaining(q0 -> 3, q1 -> 3, q2 -> 2, q3 -> 0)

  test("take(3) decreasing remaining"):
    assertEquals(rem1.take(3)._1, List(q0, q1, q2))

  test("take(5) decreasing remaining"):
    assertEquals(rem1.take(5)._1, List(q0, q0, q1, q1, q2))

  test("take(6) decreasing remaining"):
    assertEquals(rem1.take(6)._1, List(q0, q0, q1, q1, q2, q2))