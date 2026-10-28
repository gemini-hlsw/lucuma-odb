// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package imaging

import cats.data.NonEmptyList
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt

/**
 * Instrument-independent arithmetic for taking `k` consecutive exposures at
 * each offset position.  A filter's exposures are taken in "stops" of `k`
 * exposures, all in one filter at one offset position.  The last stop is
 * short when the exposure count isn't a multiple of `k`, so the exposure count
 * prescribed by the ITC is always honored exactly.  With `k` = 1 a stop is a
 * single exposure.
 */
object ExposuresPerOffset:

  /**
   * Number of stops (and therefore offset positions) needed to take `count`
   * exposures, `k` at a time.
   */
  def stopCount(count: PosInt, k: PosInt): Int =
    (count.value - 1) / k.value + 1

  /**
   * Size of each stop needed to take `count` exposures, `k` at a time.  All
   * but the last are of size `k`.
   */
  def stopSizes(count: PosInt, k: PosInt): List[Int] =
    List.tabulate(stopCount(count, k))(i => k.value min (count.value - i * k.value))

  /**
   * Number of offset positions needed for grouped (and pre-) imaging, where
   * each filter repeats the same pattern of positions, given the exposure count
   * of each filter.
   */
  def groupedOffsetCount(counts: NonEmptyList[PosInt], k: PosInt): Int =
    stopCount(counts.toList.maxBy(_.value), k)

  /**
   * Number of offset positions needed for interleaved imaging, where the
   * pattern of positions spans the whole sequence, given the exposure count of
   * each filter.
   */
  def interleavedOffsetCount(counts: NonEmptyList[PosInt], k: PosInt): Int =
    counts.toList.map(stopCount(_, k)).sum

  /**
   * Repeats each offset `k` times, keeping just enough to cover `count`
   * exposures.
   */
  def repeat[A](offsets: List[A], k: PosInt, count: PosInt): List[A] =
    offsets.iterator.flatMap(o => Iterator.fill(k.value)(o)).take(count.value).toList

  /**
   * Plans an interleaved sequence.  Each filter's stops are divided among as
   * many groups as the filter with the fewest stops has, with any remainder
   * spread over the first groups.  Each group lists its stops, in filter
   * order, as a filter and the number of exposures to take at that stop.
   *
   * @param filters filters in the order they are visited within a group,
   *                paired with the exposure count for each
   */
  def interleave[L](filters: List[(L, PosInt)], k: PosInt): List[List[(L, Int)]] =
    val stops      = filters.map((f, count) => f -> stopSizes(count, k))
    val groupCount = stops.map(_._2.size).minOption.getOrElse(0)

    stops
      .map: (f, sizes) =>
        val counts = List.tabulate(groupCount): n =>
          (sizes.size / groupCount) + (if sizes.size % groupCount > n then 1 else 0)
        counts
          .scanLeft(0)(_ + _)
          .zip(counts)
          .map((start, count) => sizes.slice(start, start + count).tupleLeft(f))
      .transpose
      .map(_.flatten)
