// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import cats.data.NonEmptyList
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import munit.Location
import munit.ScalaCheckSuite

class ScienceSuite extends ScalaCheckSuite {

  private def integrationTime(time: TimeSpan, exposures: Int): IntegrationTime =
    IntegrationTime(time, PosInt.unsafeFrom(exposures))

  import Science.Adjustment
  import Science.Goal

  private val Δλs = List(
    WavelengthDither.Zero,
    WavelengthDither.intPicometers.get(5000),
    WavelengthDither.intPicometers.get(-5000)
  )

  private val Δqs = List(
    Offset.Q.Zero,
    Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("15.0")),
    Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal("-15.0"))
  )

  private val adjs = Δλs.zip(Δqs).map(Adjustment(_, _))

  private def goals(goal: List[(Int, Int)]): List[Goal] =
    adjs.zip(goal).map { case (adj, (perBlock, total)) =>
      Goal(adj, NonNegInt.unsafeFrom(perBlock), NonNegInt.unsafeFrom(total))
    }

  case class Case(goal: Int*):
    def expected(timeSpan: TimeSpan): List[Goal] =
      goals(goal.toList.fproductLeft { total =>
        val perBlock = (Science.SciencePeriod.toMicroseconds / timeSpan.toMicroseconds).toInt
        perBlock min total
      })

  def execCases(timeSpan: TimeSpan, cases: Case*): Unit =
    cases.toList.zipWithIndex.foreach { case (c, idx) =>
      assertEquals(
        Science.Goal.compute(Δλs, Δqs, integrationTime(timeSpan, idx+1)),
        NonEmptyList.fromListUnsafe(c.expected(timeSpan)),
        s"""Failure: ${TimeSpan.format(timeSpan)}' x ${idx+1}"""
      )
    }

  test("30 min tests") {
    execCases(
      30.minTimeSpan,
      Case(1, 0, 0),
      Case(1, 1, 0),
      Case(1, 1, 1),
      Case(2, 1, 1),
      Case(2, 2, 1),
      Case(2, 2, 2),
      Case(3, 2, 2),
      Case(4, 2, 2), // <-
      Case(4, 3, 2),
      Case(4, 4, 2),
    )
  }

  test("20 min tests") {
    execCases(
      20.minTimeSpan,
      Case(1, 0, 0),
      Case(1, 1, 0),
      Case(1, 1, 1),
      Case(2, 1, 1),
      Case(2, 2, 1),
      Case(2, 2, 2),
      Case(3, 2, 2),
      Case(3, 3, 2),
      Case(3, 3, 3),
      Case(4, 3, 3),
      Case(5, 3, 3), // <-
      Case(6, 3, 3),
      Case(6, 4, 3),
      Case(6, 5, 3),
      Case(6, 6, 3),
      Case(6, 6, 4),
      Case(6, 6, 5),
      Case(6, 6, 6),
      Case(7, 6, 6)
    )
  }

  test("11 min tests") {
    execCases(
      11.minTimeSpan,
      Case(1, 0, 0),
      Case(1, 1, 0),
      Case(1, 1, 1),
      Case(2, 1, 1),
      Case(2, 2, 1),
      Case(2, 2, 2),
      Case(3, 2, 2),
      Case(3, 3, 2),
      Case(3, 3, 3),
      Case(4, 3, 3),
      Case(4, 4, 3),
      Case(4, 4, 4),
      Case(5, 4, 4),
      Case(5, 5, 4),
      Case(5, 5, 5),
      Case(6, 5, 5),
      Case(7, 5, 5), // <-
      Case(8, 5, 5),
      Case(9, 5, 5),
      Case(10, 5, 5),
      Case(10, 6, 5),
    )
  }

  test("15 min tests") {
    execCases(
      15.minTimeSpan,
      Case(1, 0, 0),
      Case(1, 1, 0),
      Case(1, 1, 1),
      Case(2, 1, 1),
      Case(2, 2, 1),
      Case(2, 2, 2),
      Case(3, 2, 2),
      Case(3, 3, 2),
      Case(3, 3, 3),
      Case(4, 3, 3),
      Case(4, 4, 3),
      Case(4, 4, 4),
      Case(5, 4, 4),
      Case(6, 4, 4), // <-
      Case(7, 4, 4),
      Case(8, 4, 4),
      Case(8, 5, 4)
    )
  }

  test("45 min tests") {
    execCases(
      45.minTimeSpan,
      Case(1, 0, 0),
      Case(1, 1, 0),
      Case(1, 1, 1),
      Case(2, 1, 1),
      Case(2, 2, 1),
      Case(2, 2, 2),
      Case(3, 2, 2),
      Case(3, 3, 2),
      Case(3, 3, 3),
      Case(4, 3, 3),
      Case(4, 4, 3),
      Case(4, 4, 4),
      Case(5, 4, 4),
      Case(5, 5, 4),
      Case(5, 5, 5),
      Case(6, 5, 5)
    )
  }
}
