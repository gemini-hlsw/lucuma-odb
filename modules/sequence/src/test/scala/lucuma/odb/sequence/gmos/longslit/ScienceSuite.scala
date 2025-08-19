// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.util.TimeSpan
import munit.Location
import munit.ScalaCheckSuite

class ScienceSuite extends ScalaCheckSuite:

  import Science.Goal
  import Science.SciencePeriod

  def plan(
    μs:    Long,
    goals: NonEmptyList[Goal]
  ): List[List[(Int, Int)]] =
    val max  = (SciencePeriod.toMicroseconds / μs).toInt

    def go(reqs: List[Remaining[Offset.Q]], idx: Int, acc: List[List[(Int, Int)]]): List[List[(Int, Int)]] =
      if reqs.foldMap(_.total.value) === 0 then acc.reverse
      else
        val (qs, req) = reqs(idx % reqs.size).take(max)
        val block =
          qs.map(q => (Angle.signedMicroarcseconds.get(q.toAngle) / 1000000).toInt)
            .tupleLeft(500 + goals.get(idx % reqs.size).get.Δλ.toPicometers.value / 1000)
        go(reqs.updated(idx % reqs.size, req), idx + 1, block :: acc)

    go(goals.toList.map(_.requirement), 0, List.empty)

  def compare(
    num:      Int,
    exptime:  Int,
    spectral: List[Int],
    spatial:  List[Int],
    expected: List[List[(Int, Int)]]
  ): Unit =
    def formatResult(res: List[List[(Int, Int)]]): String =
      res
        .map: lst =>
          lst
            .map: (wave, q) =>
              f"$wave\t$q%4d"
            .mkString("\n")
        .mkString("\n---\n")

    val μs    = exptime * 1000000L
    val goals = Goal.compute(
      spectral.map(d => WavelengthDither.intPicometers.get((d - 500) * 1000)),
      spatial.map(q => Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(q))),
      PosLong.unsafeFrom(μs),
      PosInt.unsafeFrom(num)
    )

    assertEquals(formatResult(plan(μs, goals)), formatResult(expected))

  test("--num 1 --expTime  600 --spectral 500 --spatial 0"):
    compare(
      num      = 1,
      exptime  = 600,
      spectral = List(500),
      spatial  = List(0),
      expected = List(
        List(
          500 -> 0
        )
      )
    )

  test("--num 2 --expTime  600 --spectral 500 --spatial 0"):
    compare(
      num      = 2,
      exptime  = 600,
      spectral = List(500),
      spatial  = List(0),
      expected = List(
        List(
          500 -> 0,
          500 -> 0
        )
      )
    )

  test("--num 4 --expTime 1200 --spectral 500 --spatial 0"):
    compare(
      num      = 4,
      exptime  = 1200,
      spectral = List(500),
      spatial  = List(0),
      expected = List(
        List(
          500 -> 0,
          500 -> 0,
          500 -> 0
        ),
        List(
          500 -> 0
        )
      )
    )

  test("--num 4 --expTime 1200 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 4,
      exptime  = 1200,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 -> 0,
          500 -> 15,
        ),
        List(
          505 -> -15
        ),
        List(
          495 -> 0
        )
      )
    )

  test("--num 9 --expTime 1200 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 9,
      exptime  = 1200,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 -> 0,
          500 -> 15,
          500 -> -15
        ),
        List(
          505 -> 0,
          505 -> 15,
          505 -> -15
        ),
        List(
          495 -> 0,
          495 -> 15,
          495 -> -15
        )
      )
    )

  test("--num 11 --expTime 1200 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 11,
      exptime  = 1200,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 -> 0,
          500 -> 15,
          500 -> -15
        ),
        List(
          505 -> 0,
          505 -> 15,
          505 -> -15
        ),
        List(
          495 -> 0,
          495 -> 15,
          495 -> -15
        ),
        List(
          500 -> 0,
          500 -> 15
        )
      )
    )

  test("--num 13 --expTime 1200 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 13,
      exptime  = 1200,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 -> 0,
          500 -> 15,
          500 -> -15
        ),
        List(
          505 -> 0,
          505 -> 15,
          505 -> -15
        ),
        List(
          495 -> 0,
          495 -> 15,
          495 -> -15
        ),
        List(
          500 -> 0,
          500 -> 15,
          500 -> -15
        ),
        List(
          505 -> 0
        )
      )
    )

  test("--num 13 --expTime 1201 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 13,
      exptime  = 1201,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 ->  0,
          500 -> 15
        ),
        List(
          505 -> -15,
          505 ->   0
        ),
        List(
          495 ->   0,
          495 ->  15
        ),
        List(
          500 -> -15,
          500 ->   0
        ),
        List(
          505 ->  15,
          505 -> -15
        ),
        List(
          495 -> -15,
          495 ->   0
        ),
        List(
          500 -> 15
        )
      )
    )

  test("--num 10 --expTime 600 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 10,
      exptime  = 600,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 ->   0,
          500 ->   0,
          500 ->  15,
          500 -> -15
        ),
        List(
          505 ->   0,
          505 ->  15,
          505 -> -15
        ),
        List(
          495 ->   0,
          495 ->  15,
          495 -> -15
        )
      )
    )

  test("--num 20 --expTime 600 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 20,
      exptime  = 600,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 ->   0,
          500 ->   0,
          500 ->  15,
          500 ->  15,
          500 -> -15,
          500 -> -15
        ),
        List(
          505 ->   0,
          505 ->   0,
          505 ->  15,
          505 ->  15,
          505 -> -15,
          505 -> -15
        ),
        List(
          495 ->   0,
          495 ->   0,
          495 ->  15,
          495 ->  15,
          495 -> -15,
          495 -> -15
        ),
        List(
          500 ->   0,
          500 ->  15
        )
      )
    )

  test("--num 2 --expTime 1200 --spectral 500,505,495 --spatial 0,15,-15"):
    compare(
      num      = 2,
      exptime  = 1200,
      spectral = List(500,505,495),
      spatial  = List(0,15,-15),
      expected = List(
        List(
          500 ->   0
        ),
        List(
          505 ->  15
        )
      )
    )