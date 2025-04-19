// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.all.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import munit.FunSuite

class ContiguousTimestampMapSuite extends FunSuite {
  private def timestamp(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli).get

  private def interval(epochMilli0: Long, epochMilli1: Long): TimestampInterval =
    TimestampInterval.between(timestamp(epochMilli0), timestamp(epochMilli1))

  private def tuple(epochMilli0: Long, epochMilli1: Long, s: String): (TimestampInterval, String) =
    (interval(epochMilli0, epochMilli1), s)

  extension (ctm: ContiguousTimestampMap[String])
    private def toTuple: Option[(Option[TimestampInterval], List[(TimestampInterval, String)])] =
      ctm.some.toTuple

  extension (ctm: Option[ContiguousTimestampMap[String]])
    private def toTuple: Option[(Option[TimestampInterval], List[(TimestampInterval, String)])] =
      ctm.map(c => (c.coverage, c.intervals.toList))

  private def toCompare(epochMilli0: Long, epochMilli1: Long, is: (Long, Long, String)*): 
    Option[(Option[TimestampInterval], List[(TimestampInterval, String)])] =
      (interval(epochMilli0, epochMilli1).some, is.toList.map( (milli1, milli2, s) => (interval(milli1, milli2), s))).some

  test("fromList") {
    val ctm = 
      ContiguousTimestampMap.fromList(
        List(tuple(3, 5, "a"), tuple(1, 2, "b"), tuple(5, 7, "a"), tuple(2, 3, "c"))
      )

    assertEquals(ctm.toTuple, toCompare(1, 7, (1, 2, "b"), (2, 3, "c"), (3, 7, "a")))

    val ctmNone = 
      ContiguousTimestampMap.fromList(
        List(tuple(3, 5, "a"), tuple(1, 2, "b"), tuple(5, 7, "a"))
      )
    assertEquals(ctmNone, None) 

    val ctmEmpty = ContiguousTimestampMap.fromList(List.empty[(TimestampInterval, String)])
    assert(ctmEmpty.exists(_.isEmpty), "Empty list makes empty map")
  }

  test("add") {
    val ctm = 
      ContiguousTimestampMap.fromList(
        List(tuple(7, 11, "b"), tuple(5, 7, "a"))
      ).get

    assertEquals(ctm.add(interval(3, 5), "a").toTuple, toCompare(3, 11, (3, 7, "a"), (7, 11, "b")))
    assertEquals(ctm.add(interval(3, 5), "_").toTuple, toCompare(3, 11, (3, 5, "_"), (5, 7, "a"), (7, 11, "b")))
    assertEquals(ctm.add(interval(3, 5), "b").toTuple, toCompare(3, 11, (3, 5, "b"), (5, 7, "a"), (7, 11, "b")))
    assertEquals(ctm.add(interval(11, 15), "b").toTuple, toCompare(5, 15, (5, 7, "a"), (7, 15, "b")))
    assertEquals(ctm.add(interval(11, 15), "c").toTuple, toCompare(5, 15, (5, 7, "a"), (7, 11, "b"), (11, 15, "c")))
    assertEquals(ctm.add(interval(11, 15), "a").toTuple, toCompare(5, 15, (5, 7, "a"), (7, 11, "b"), (11, 15, "a")))

    assertEquals(ctm.add(interval(2, 4), "a"), None)
    assertEquals(ctm.add(interval(2, 4), "_"), None)
    assertEquals(ctm.add(interval(12, 14), "b"), None)
    assertEquals(ctm.add(interval(12, 14), "c"), None)

    assertEquals(
      ContiguousTimestampMap.empty[String].add(interval(3, 5), "a").toTuple, 
      toCompare(3, 5, (3, 5, "a"))
    )
  }

  test("union") {
    val ctm = 
      ContiguousTimestampMap.fromList(
        List(tuple(7, 11, "b"), tuple(5, 7, "a"))
      ).get
    val abutsSame = 
      ContiguousTimestampMap.fromList(
        List(tuple(11, 15, "b"), tuple(15, 17, "a"))
      ).get
    val abutsDiffers = 
      ContiguousTimestampMap.fromList(
        List(tuple(11, 15, "d"), tuple(15, 17, "a"))
      ).get
    val disjoint = 
      ContiguousTimestampMap.fromList(
        List(tuple(12, 15, "d"), tuple(15, 17, "a"))
      ).get
    val overlaps = 
      ContiguousTimestampMap.fromList(
        List(tuple(10, 15, "d"), tuple(15, 17, "a"))
      ).get
    val empty = ContiguousTimestampMap.empty[String]
    
    assertEquals(ctm.union(abutsSame).toTuple, toCompare(5, 17, (5, 7, "a"), (7, 15, "b"), (15, 17, "a"))) 
    assertEquals(abutsSame.union(ctm).toTuple, toCompare(5, 17, (5, 7, "a"), (7, 15, "b"), (15, 17, "a"))) 
    assertEquals(ctm.union(abutsDiffers).toTuple, toCompare(5, 17, (5, 7, "a"), (7, 11, "b"), (11, 15, "d"), (15, 17, "a"))) 
    assertEquals(abutsDiffers.union(ctm).toTuple, toCompare(5, 17, (5, 7, "a"), (7, 11, "b"), (11, 15, "d"), (15, 17, "a"))) 

    assertEquals(ctm.union(disjoint), None)
    assertEquals(disjoint.union(ctm), None)
    assertEquals(ctm.union(overlaps), None)
    assertEquals(overlaps.union(ctm), None)

    assertEquals(ctm.union(empty).toTuple, toCompare(5, 11, (5, 7, "a"), (7, 11, "b")))
    assertEquals(empty.union(ctm).toTuple, toCompare(5, 11, (5, 7, "a"), (7, 11, "b")))
  }

  test("slice") {
    val ctm = 
      ContiguousTimestampMap.fromList(
        List(tuple(7, 11, "b"), tuple(5, 7, "a"))
      ).get

    // superset
    assertEquals(ctm.slice(interval(3, 15)).toTuple, ctm.toTuple)
    assertEquals(ctm.slice(interval(5, 15)).toTuple, ctm.toTuple)
    assertEquals(ctm.slice(interval(3, 11)).toTuple, ctm.toTuple)

    // equals
    assertEquals(ctm.slice(interval(5, 11)).toTuple, ctm.toTuple)

    // upper partial
    assertEquals(ctm.slice(interval(3, 9)).toTuple, toCompare(5, 9, (5, 7, "a"), (7, 9, "b")))
    assertEquals(ctm.slice(interval(3, 7)).toTuple, toCompare(5, 7, (5, 7, "a")))
    assertEquals(ctm.slice(interval(3, 6)).toTuple, toCompare(5, 6, (5, 6, "a")))

    // lower partial
    assertEquals(ctm.slice(interval(6, 19)).toTuple, toCompare(6, 11, (6, 7, "a"), (7, 11, "b")))
    assertEquals(ctm.slice(interval(7, 19)).toTuple, toCompare(7, 11, (7, 11, "b")))
    assertEquals(ctm.slice(interval(9, 19)).toTuple, toCompare(9, 11, (9, 11, "b")))

    // abuts
    assertEquals(ctm.slice(interval(3, 5)).toTuple, (None, Nil).some)
    assertEquals(ctm.slice(interval(11, 19)).toTuple, (None, Nil).some)

    // disjoint
    assertEquals(ctm.slice(interval(1, 4)).toTuple, (None, Nil).some) 
    assertEquals(ctm.slice(interval(12, 19)).toTuple, (None, Nil).some)
  }

  test("findMissingIntervals") {
    val ctm = 
      ContiguousTimestampMap.fromList(
        List(tuple(7, 11, "b"), tuple(5, 7, "a"))
      ).get

    // superset
    assertEquals(ctm.findMissingIntervals(interval(3, 15)), List(interval(3, 5), interval(11, 15)))
    assertEquals(ctm.findMissingIntervals(interval(5, 15)), List(interval(11, 15)))
    assertEquals(ctm.findMissingIntervals(interval(3, 11)), List(interval(3, 5)))

    // equals
    assertEquals(ctm.findMissingIntervals(interval(5, 11)), Nil)

    // upper partial
    assertEquals(ctm.findMissingIntervals(interval(3, 9)), List(interval(3, 5)))
    assertEquals(ctm.findMissingIntervals(interval(3, 7)), List(interval(3, 5)))
    assertEquals(ctm.findMissingIntervals(interval(3, 6)), List(interval(3, 5)))

    // lower partial
    assertEquals(ctm.findMissingIntervals(interval(6, 19)), List(interval(11, 19)))
    assertEquals(ctm.findMissingIntervals(interval(7, 19)), List(interval(11, 19)))
    assertEquals(ctm.findMissingIntervals(interval(9, 19)), List(interval(11, 19)))

    // abuts
    assertEquals(ctm.findMissingIntervals(interval(3, 5)), List(interval(3, 5)))
    assertEquals(ctm.findMissingIntervals(interval(11, 19)), List(interval(11, 19)))

    // disjoint
    assertEquals(ctm.findMissingIntervals(interval(1, 4)), List(interval(1, 5)))
    assertEquals(ctm.findMissingIntervals(interval(12, 19)), List(interval(11, 19)))

    // empty
    assertEquals(
      ContiguousTimestampMap.empty[String].findMissingIntervals(interval(3, 9)),
      List(interval(3, 9))
    )
  }
}
