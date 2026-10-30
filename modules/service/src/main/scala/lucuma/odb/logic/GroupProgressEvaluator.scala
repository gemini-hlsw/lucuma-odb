// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.model.group.GroupProgress
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.GroupTree

/**
 * Evaluates the progress of groups toward satisfying their constraints.
 *
 * The rules:
 *
 *   - An inactive observation that isn't complete is left out of its group
 *     entirely, as is a complete one with no completion time (declared
 *     complete without ever having been observed) and a subgroup left with no
 *     elements.  A group with no elements is empty.
 *   - A group requires `minRequired` elements, or all of them when unset.
 *     An ordered group requires the first ones, in order.  A subgroup counts
 *     as one element, complete when its own status is complete.
 *   - Only the elements that count toward the group are checked.  An ordered
 *     group's elements after the first `minRequired` never count.  An
 *     unordered group counts all of its elements until enough are complete,
 *     and from then on only the completions that met its minimum (in
 *     completion order): anything observed afterwards is extra, and can't
 *     make the group unschedulable.
 *     A complete subgroup is summarized for its parent (its start, nights and
 *     sites) from the elements that counted.
 *   - Zero intervals are treated as unset.  The intervals are checked between
 *     consecutive elements in completion order: the later one must start no
 *     earlier than the minimum interval after the earlier one completes, and
 *     complete no later than the maximum interval after it.  An element still
 *     executing is checked against the last completion as soon as it starts.
 *   - A same-night group must have all of its observations at one site, and
 *     its completions must fall on the same observing night there.
 *   - A group with observations-per-night limits must likewise have all of
 *     its observations at one site.  Every completion of its elements falls
 *     on the observing night of its completion time there (a subgroup, which
 *     is only ever a per-observation calibration group, completing with the
 *     last of its own), extras included, through the night the group
 *     completes.  Later ones are ignored.  A night with any completions must
 *     end with at least the minimum, and none may hold more than the maximum.
 *     Whether the nights to come can satisfy the limits isn't predicted: a
 *     group that can't finish is unschedulable once a night comes up short.
 *     A group with enough completions stays ongoing while the night in
 *     progress (that of the last completion, until it ends) is short of the
 *     minimum and may still take an extra.
 *   - Any violation makes the group unschedulable, including a deadline that
 *     `now` has passed: the maximum interval since the last completion, or the
 *     end of the completions' night with elements still to observe.  A night
 *     that ends before the minimum interval has passed is missed already.
 *     A night in progress short of the minimum is settled once the group has
 *     enough completions and too few elements remain to make up the night.
 */
object GroupProgressEvaluator:

  /**
   * What the evaluator needs to know about an observation.  `inactive` is
   * true when a user has set it inactive (or declined it, for a telluric).
   * `startTime` is when it started executing, once it has started.
   * `completionTime` is when it finished, once it is complete, and is unknown
   * only for an observation declared complete without ever having been
   * observed.  `site` is where it was or would be observed, if known.
   */
  final case class ObservationFacts(
    executionState: ExecutionState,
    inactive:       Boolean,
    startTime:      Option[Timestamp],
    completionTime: Option[Timestamp],
    site:           Option[Site]
  )

  object ObservationFacts:
    val NotStarted: ObservationFacts =
      ObservationFacts(ExecutionState.NotStarted, false, None, None, None)

  /**
   * Evaluates every group in each of the trees, which should hold no deleted
   * elements.  An observation missing from `facts` is taken as not started.
   */
  def evaluate(
    trees: List[GroupTree.Branch],
    facts: Map[Observation.Id, ObservationFacts],
    now:   Timestamp
  ): Map[Group.Id, GroupProgress] =
    trees.flatMap(evaluateBranch(_, facts, now)._2).toMap

  // A group element as its parent sees it: an observation, or a subgroup
  // summarized.  `excluded` elements are left out of the group entirely.
  // `completion` is when it completed, once it has.  `nights` are the nights
  // of the completions it counts, and `sites` the sites of the observations
  // it holds.
  private final case class Element(
    excluded:      Boolean,
    completion:    Option[Timestamp],
    unschedulable: Boolean,
    started:       Boolean,
    startTime:     Option[Timestamp],
    nights:        Set[LocalObservingNight],
    sites:         Set[Site]
  ):
    def complete: Boolean =
      completion.isDefined

    def isRemaining: Boolean =
      !complete && !unschedulable

  private def leafElement(leaf: ObservationFacts): Element =
    import ExecutionState.*
    val complete = leaf.executionState match
      case Completed | DeclaredComplete => true
      case _                            => false
    val started  = leaf.executionState match
      case NotDefined | NotStarted => false
      case _                       => true
    val time     = Option.when(complete)(leaf.completionTime).flatten
    // Left out when inactive and incomplete, or complete with no known time.
    val excluded = if complete then time.isEmpty else leaf.inactive

    Element(
      excluded      = excluded,
      completion    = time,
      unschedulable = false,
      started       = started,
      startTime     = Option.when(started)(leaf.startTime).flatten,
      nights        = (for t <- time; s <- leaf.site yield night(s, t)).toSet,
      sites         = if excluded then Set.empty else leaf.site.toSet
    )

  private def night(site: Site, time: Timestamp): LocalObservingNight =
    LocalObservingNight.fromSiteAndInstant(site, time.toInstant)

  private def nightEnd(n: LocalObservingNight, site: Site): Timestamp =
    Timestamp.fromInstantTruncatedAndBounded(n.atSite(site).end)

  private def evaluateBranch(
    branch: GroupTree.Branch,
    facts:  Map[Observation.Id, ObservationFacts],
    now:    Timestamp
  ): (Element, Map[Group.Id, GroupProgress]) =

    val evaluated: List[(Element, Map[Group.Id, GroupProgress])] =
      branch.children.map:
        case GroupTree.Leaf(oid) => (leafElement(facts.getOrElse(oid, ObservationFacts.NotStarted)), Map.empty)
        case b: GroupTree.Branch => evaluateBranch(b, facts, now)

    val nested: Map[Group.Id, GroupProgress] =
      evaluated.flatMap(_._2).toMap

    val elements: List[Element] =
      evaluated.map(_._1).filterNot(_.excluded)

    val (progress, element) =
      NonEmptyList.fromList(elements).fold((GroupProgress.Empty, EmptyElement))(evaluateElements(branch, _, now))

    (element, nested + (branch.groupId -> progress))

  // An empty group, which its parent leaves out.
  private val EmptyElement: Element =
    Element(
      excluded      = true,
      completion    = None,
      unschedulable = false,
      started       = false,
      startTime     = None,
      nights        = Set.empty,
      sites         = Set.empty
    )

  private def evaluateElements(
    branch: GroupTree.Branch,
    viable: NonEmptyList[Element],
    now:    Timestamp
  ): (GroupProgress, Element) =

    val minInterval = branch.minInterval.filterNot(_.isZero)
    val maxInterval = branch.maxInterval.filterNot(_.isZero)
    // The minimum, or all of the elements when it is unset (or zero, which the
    // database forbids and would mean unset).  Either way, at least one.
    val required: PosInt =
      branch.minRequired.flatMap(n => PosInt.from(n.value.toInt).toOption).getOrElse(PosInt.unsafeFrom(viable.length))

    // The elements that count toward the group, which everything below
    // considers.  An ordered group requires its first `required` elements, so
    // later ones never count.  An unordered group counts all of them until
    // enough are complete, and then only the earliest completions that met
    // the minimum.
    val elements: List[Element] =
      if branch.ordered then viable.toList.take(required.value)
      else
        val done = viable.filter(_.complete)
        if done.size < required.value then viable.toList
        else done.sortBy(_.completion).take(required.value)

    // The completions, in order.
    val completions: List[(Timestamp, Element)] =
      elements.mapFilter(e => e.completion.tupleRight(e)).sortBy(_._1)

    val completed: List[Element]  = completions.map(_._2)
    val timed: List[Timestamp]    = completions.map(_._1)
    val remaining: List[Element]  = elements.filter(_.isRemaining)

    // Each completion but the first, with the time of the completion before
    // it.
    val consecutive: List[(Timestamp, Element)] =
      completions.zip(completions.drop(1)).map { case ((t0, _), (_, e1)) => (t0, e1) }

    val nights: Set[LocalObservingNight] =
      completed.flatMap(_.nights).toSet

    val sites: Set[Site] =
      elements.flatMap(_.sites).toSet

    // The end of the completions' night, for a same-night group with a single
    // completion night.  A group spanning sites is a violation of its own, so
    // the group has one site, known from its completions even when the
    // observations still to do have no instrument yet.
    val sameNightEnd: Option[Timestamp] =
      Option.when(branch.sameNight && nights.size == 1 && sites.size == 1)(
        nightEnd(nights.head, sites.head)
      )

    val completedCount: NonNegInt = NonNegInt.unsafeFrom(completed.size)

    // Whether enough of the elements that count are complete (for an ordered
    // group, those are its first `required`, so all of them).
    val countMet: Boolean =
      completed.size >= required.value

    // Observations per night.  The limits apply only at a single site, where
    // each completion falls on one night.  Every completion of the group's
    // elements counts toward its night, extras included, until the group
    // completes, and then those on the night it completes.  The night in
    // progress, if any, is that of the last of these, until it ends.
    val perNight: Boolean =
      branch.minObsPerNight.isDefined || branch.maxObsPerNight.isDefined

    val minPerNight: Int         = branch.minObsPerNight.fold(1)(_.value.toInt)
    val maxPerNight: Option[Int] = branch.maxObsPerNight.map(_.value.toInt)

    // The elements still to observe, extras included.
    val available: Int =
      viable.toList.count(_.isRemaining)

    val perNightCompletions: List[(Timestamp, Element)] =
      val all = viable.toList.mapFilter(e => e.completion.tupleRight(e))
      if !countMet then all
      else
        (for
          s <- Option.when(sites.size == 1)(sites.head)
          t <- timed.lastOption
          e  = nightEnd(night(s, t), s)
        yield all.filter(_._1 < e)).getOrElse(completions)

    val perNightSites: Set[Site] =
      if countMet then sites ++ perNightCompletions.flatMap(_._2.sites)
      else viable.toList.flatMap(_.sites).toSet

    val perNightSite: Option[Site] =
      Option.when(perNight && perNightSites.size == 1)(perNightSites.head)

    val countsByNight: Map[LocalObservingNight, Int] =
      perNightSite.fold(Map.empty): s =>
        perNightCompletions.groupMapReduce((t, _) => night(s, t))(_ => 1)(_ + _)

    val openNight: Option[(LocalObservingNight, Timestamp)] =
      for
        s <- perNightSite
        t <- perNightCompletions.map(_._1).maxOption
        n  = night(s, t)
        e  = nightEnd(n, s)
        if now < e
      yield (n, e)

    val openCount: Int =
      openNight.fold(0)((n, _) => countsByNight.getOrElse(n, 0))

    // The night in progress is short of the minimum, and so must take more.
    val openNightShort: Boolean =
      openNight.isDefined && openCount < minPerNight

    // Whether `later` started before the minimum interval had passed since a
    // completion at `t`.
    def startedTooSoon(t: Timestamp, later: Element): Boolean =
      (for
        min <- minInterval
        s   <- later.startTime
      yield s < (t +| min)).getOrElse(false)

    // An element completed before an earlier one: either the earlier one is
    // still incomplete or it completed later.
    def outOfOrder: Boolean =
      elements.zipWithIndex.exists: (later, j) =>
        later.completion.exists: tl =>
          elements.take(j).exists(_.completion.forall(_ > tl))

    // The earliest the next element may start, per the minimum interval.
    val nextStart: Option[Timestamp] =
      timed.lastOption.map(t => minInterval.fold(t)(t +| _))

    val started: Boolean =
      elements.exists(_.started)

    // The window ends at `Timestamp.Max` when nothing limits its end.  (There
    // is no window that would end before it starts, rather than one with its
    // ends swapped, but then the group is unschedulable anyway: the minimum
    // interval never exceeds the maximum, and a night that ends too soon is a
    // violation below.)
    //
    // A night in progress short of the minimum must take the next element, so
    // it limits the window's end, and one at the maximum can't, so the window
    // starts when it ends.
    val nextWindow: Option[TimestampInterval] =
      val perNightEnd: Option[Timestamp] =
        openNight.collect { case (_, e) if openCount < minPerNight => e }
      val perNightStart: Option[Timestamp] =
        openNight.collect { case (_, e) if maxPerNight.exists(openCount >= _) => e }
      for
        last   <- timed.lastOption
        start0 <- nextStart
        if minInterval.isDefined || maxInterval.isDefined || branch.sameNight || perNight
        start   = (start0 :: perNightStart.toList).max
        end     = (maxInterval.map(last +| _).toList ++ sameNightEnd.toList ++ perNightEnd.toList).minOption.getOrElse(Timestamp.Max)
        if start <= end
      yield TimestampInterval.between(start, end)

    // A night short of the minimum: one that has ended, or the night in
    // progress once it can take no more (the minimum interval reaches past its
    // end, or the group has enough completions and too few elements remain to
    // make up the night).
    def perNightShort: Boolean =
      countsByNight.exists: (n, c) =>
        c < minPerNight && openNight.forall { (o, e) =>
          o =!= n || nextStart.exists(_ >= e) || (countMet && available < minPerNight - c)
        }

    import GroupConstraintViolation.*

    // The constraints that can no longer be met.  An unschedulable subgroup
    // sinks the group only if the group needs it.  Each completion is checked
    // against the one before it: it must have started after the minimum
    // interval and completed within the maximum.  An element still executing
    // comes after the last completion, and starting too soon can't be undone,
    // so it is checked as soon as it starts.  The deadlines (the maximum
    // interval, and the end of a same-night group's night, which the minimum
    // interval may already reach) matter only while the group is short of
    // completion.
    val violations: List[GroupConstraintViolation] =
      List(
        SubgroupUnschedulable  -> (
          if branch.ordered then elements.exists(_.unschedulable)
          else elements.count(!_.unschedulable) < required.value && elements.exists(_.unschedulable)
        ),
        InsufficientElements   -> (elements.size < required.value),
        Order                  -> (branch.ordered && outOfOrder),
        MinimumInterval        -> (
          consecutive.exists(startedTooSoon) ||
          timed.lastOption.exists(t => remaining.exists(startedTooSoon(t, _)))
        ),
        MaximumInterval        -> (
          maxInterval.exists: max =>
            timed.zip(timed.drop(1)).exists((t0, t1) => TimeSpan.between(t0, t1) > max) ||
            (!countMet && timed.lastOption.exists(t => now > (t +| max)))
        ),
        SameNightMultipleSites -> (branch.sameNight && sites.size > 1),
        SameNight              -> (
          branch.sameNight && (
            nights.size > 1 ||
            (!countMet && sameNightEnd.exists(end => now >= end || nextStart.exists(_ >= end)))
          )
        ),
        ObservationsPerNightMultipleSites -> (perNight && perNightSites.size > 1),
        ObservationsPerNightMinimum       -> (perNight && perNightShort),
        ObservationsPerNightMaximum       -> (perNight && maxPerNight.exists(max => countsByNight.values.exists(_ > max)))
      ).collect { case (v, true) => v }

    val progress: GroupProgress =
      NonEmptyList.fromList(violations) match
        case Some(vs)         => GroupProgress.Unschedulable(completedCount, required, vs.toNes)
        case None if countMet && !openNightShort => GroupProgress.Complete(completedCount, required)
        case None if started  => GroupProgress.Ongoing(completedCount, required, nextWindow)
        case None             => GroupProgress.Pending(required)

    val status: GroupStatus = progress.status

    // Its start, nights and sites, for its parent, are those of the elements
    // that counted, and it completed with the last of the completions that
    // counted.  (Complete, it has at least one.)
    val element = Element(
      excluded      = false,
      completion    = Option.when(status === GroupStatus.Complete)(timed.lastOption).flatten,
      unschedulable = status === GroupStatus.Unschedulable,
      started       = started,
      startTime     = elements.flatMap(_.startTime).minOption,
      nights        = nights,
      sites         = sites
    )

    (progress, element)
