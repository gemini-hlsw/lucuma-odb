// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.Order.*
import cats.syntax.all.*
import lucuma.core.util.TimestampInterval

import scala.collection.immutable.SortedMap

final case class ContiguousTimestampMap[A: Eq] private (
  val coverage: Option[TimestampInterval],
  val intervals:  SortedMap[TimestampInterval, A] 
) {
  import ContiguousTimestampMap.*

  def add(interval: TimestampInterval, a: A): Option[ContiguousTimestampMap[A]] =
    coverage.fold(ContiguousTimestampMap(interval.some, intervals + (interval -> a)).some) { cov =>
      if (interval.abuts(cov)) {
        val newCoverage   = cov.span(interval)
        val fromIntervals = if (interval < cov) intervals.head else intervals.last

        if (fromIntervals._2 === a)
          ContiguousTimestampMap(newCoverage.some, intervals - fromIntervals._1 + (fromIntervals._1.span(interval) -> a)).some
        else ContiguousTimestampMap(newCoverage.some, intervals + (interval -> a)).some
      }
      else none
    }

  def unsafeAdd(interval: TimestampInterval, a: A): ContiguousTimestampMap[A] = add(interval, a).get

  def union(other: ContiguousTimestampMap[A]): Option[ContiguousTimestampMap[A]] =
    (this.coverage, other.coverage) match {
      case (Some(p1), Some(p2)) if (p1.abuts(p2)) => 
        val (first, second) = if (p1 < p2) (this, other) else (other, this)
        first.intervals.toList.foldRight(second){ case ((i, a), acc) => acc.unsafeAdd(i, a) }.some
      case (Some(_), Some(_))                     =>  none
      case (_, None)                              => this.some
      case (None, _)                              => other.some
    }

  def slice(requestedPeriod: TimestampInterval): ContiguousTimestampMap[A] = {
    coverage.fold(this){ cov =>
      if (cov.intersects(requestedPeriod))
        val newPeriods: List[(TimestampInterval, A)] =
          intervals.toList.map((interval, a) => interval.intersection(requestedPeriod).map((_, a))).flatten 
        ContiguousTimestampMap.fromList(newPeriods).get
      else ContiguousTimestampMap.empty
    }
  }

  // Given the requested time interval, we need to figure out what time intervals
  // we need to create the entire requested period. This could be zero, one or 2 intervals.
  def findMissingIntervals(requestedPeriod: TimestampInterval): List[TimestampInterval] = 
    coverage.fold(List(requestedPeriod)){ cov =>
      requestedPeriod.abutWith(cov).fold(requestedPeriod.minus(cov))(List(_))
    }

  def isEmpty: Boolean = coverage.isEmpty
}

object ContiguousTimestampMap {
  def empty[A: Eq]: ContiguousTimestampMap[A] = ContiguousTimestampMap(none, SortedMap.empty)

  def single[A: Eq](period: TimestampInterval, a: A): ContiguousTimestampMap[A] =
    ContiguousTimestampMap(period.some, SortedMap.from(List((period, a))))

  def fromList[A: Eq](list: List[(TimestampInterval, A)]): Option[ContiguousTimestampMap[A]] =
    list.sortBy(_._1).foldRight(empty.some){ case ((i, a), ectm) => ectm.flatMap(_.add(i, a)) }

  extension (self: TimestampInterval)
    // expand the interval to abut other if they don't intersect
    def abutWith(other: TimestampInterval): Option[TimestampInterval] =
      if (self.intersects(other)) none
      else if (self.abuts(other)) self.some
      else if (self < other) TimestampInterval.between(self.start, other.start).some
      else TimestampInterval.between(other.end, self.end).some
}
