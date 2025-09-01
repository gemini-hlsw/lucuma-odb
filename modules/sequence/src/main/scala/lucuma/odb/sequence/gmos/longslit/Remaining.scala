// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSeqMap

// This is used for GMOS long slit for now.  If it is needed elsewhere I'll
// move it.

/**
 * 'Remaining' tracks how many instances of a particular class of items are
 * remaining in a sequence execution.  It provides a `take` method to remove the
 * `n` most prevalent items.  For example, in GMOS long slit sequence generation
 * each wavelength dither is alloted a number of datasets for each spatial
 * offset.
 */
sealed trait Remaining[A]:

  /**
   * Get the count of remaining values `a` (if any).
   */
  def get(a: A): Option[NonNegInt]

  def isEmpty: Boolean

  /**
   * Total remaining items across all instances.
   */
  def total: NonNegInt

  /**
   * Marks one use of `a`, assuming there is at least one left.
   */
  def decrement(a: A): Remaining[A] =
    decrementAll(Map(a -> 1))

  /**
   * Marks (potentially multiple) usages of items all at once.
   */
  def decrementAll(m: Map[A, Int]): Remaining[A]

  /**
   * Takes the most prevalent `n` items that are remaining.  If an item ties for
   * most prevalent, then the least used item is selected.  If there is still a
   * tie among multiple choices, the order of items at creation time is
   * respected.
   */
  def take(n: Int): (List[A], Remaining[A])

  def toMap: Map[A, NonNegInt]

object Remaining:

  private case class Counts(
    completed: NonNegInt,
    goal:      NonNegInt
  ):
    def remaining: NonNegInt =
      NonNegInt.unsafeFrom((goal.value - completed.value) max 0)

    def complete(n: Int): Counts =
      copy(completed = NonNegInt.unsafeFrom(completed.value + (n max 0)))

  private object Counts:
    def fromGoal(goal: NonNegInt): Counts =
      Counts(NonNegInt.MinValue, goal)

  def empty[A]: Remaining[A] =
    Impl(TreeSeqMap.empty[A, Counts], TreeMap.empty[NonNegInt, Set[A]], Order.allEqual[A])

  private case class Impl[A](
    elementMap:    Map[A, Counts],
    remainingMap:  TreeMap[NonNegInt, Set[A]],
    sequenceOrder: Order[A]
  ) extends Remaining[A]:

    override def get(a: A): Option[NonNegInt] =
      elementMap.get(a).map(_.remaining)

    override def isEmpty: Boolean =
      remainingMap.forall(_._1.value === 0)

    override def total: NonNegInt =
      NonNegInt.unsafeFrom:
        remainingMap.toList.foldMap((n, as) => n.value * as.size)

    override def decrementAll(m: Map[A, Int]): Remaining[A] =
      m.foldLeft(this) { case (r, (a, i)) => r.decrementImpl(a, i) }

    private def decrementImpl(a: A, n: Int): Impl[A] =
      val nOpt = elementMap.get(a)
      val mOpt = nOpt.map(_.complete(n))  // reducing remaining by n

      val elementMapʹ   = mOpt.fold(elementMap)(elementMap.updated(a, _))
      val remainingMapʹ = nOpt.fold(remainingMap): cur =>

        // Remove from the current count entry
        val as      = remainingMap(cur.remaining).excl(a)
        val removed = if as.isEmpty then remainingMap.removed(cur.remaining) else remainingMap.updated(cur.remaining, as)

        // Add to next count entry
        mOpt.fold(removed): m =>
          removed.updatedWith(m.remaining)(_.fold(Set(a))(_.incl(a)).some)

      Impl(elementMapʹ, remainingMapʹ, sequenceOrder)

    private def take1: Option[(A, Impl[A])] =

      // When deciding between multiple options for which there is an equal
      // remaining count, first pick the one that has been selected the least
      // so far.  Failing that, pick the one that was first defined in the list.
      val order = Order.whenEqual(
        Order.by[A, Int](a => elementMap.get(a).map(_.completed.value).getOrElse(0)),
        sequenceOrder
      ).toOrdering

      remainingMap.lastOption.filter(_._1.value > 0).flatMap: (_, as) => // selects the group with the most remaining
        as.minOption(using order).fproduct(a => decrementImpl(a, 1))

    override def take(n: Int): (List[A], Remaining[A]) =
      @annotation.tailrec
      def go(nʹ: Int, acc: TreeSeqMap[A, Int], r: Impl[A]): (TreeSeqMap[A, Int], Remaining[A]) =
        if nʹ <= 0 then (acc, r)
        else r.take1 match
          case None          => (acc, r)
          case Some((k, rʹ)) => go(nʹ - 1, acc.updatedWith(k)(_.fold(1)(_+1).some), rʹ)

      val (as, r) = go(n, TreeSeqMap.empty, this)
      (as.toList.flatMap((a,f) => List.fill(f)(a)), r)

    override def toMap: Map[A, NonNegInt] =
      elementMap
        .map((a, c) => (a, c.remaining))
        .toMap

  end Impl

  /**
   * Initializes a `Remaining` instance with the given counts per entry.  The
   * order of entries specified determines selection order when there is a tie
   * among multiple items in terms of remaining prevalency and past usage. If an
   * entry is duplicated, the total is added but the order is not impacted.
   */
  def apply[A](elems: (A, Int)*): Remaining[A] =
    from(elems.toList)

  def from[A](col: scala.collection.IterableOnce[(A, Int)]): Remaining[A] =
    val sums     = col.iterator.toList.groupMapReduce(_._1)(_._2)(_ + _)
    val order    = col.iterator.toList.map(_._1).distinct
    val elements = order.fproduct(sums).collect:
      case (a, n) if n >= 0 => (a, NonNegInt.unsafeFrom(n))

    Impl(
      elements.map((a, p) => (a, Counts.fromGoal(p))).toMap,
      TreeMap.from(elements.groupMapReduce(_._2)((a, _) => Set(a))(_ ++ _)),
      Order.by(elements.map(_._1).zipWithIndex.toMap)
    )