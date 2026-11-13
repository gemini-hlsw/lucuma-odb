// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.ObservationQaState
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.group.GroupConstraintViolation
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.core.model.group.GroupProgress
import lucuma.core.model.group.GroupStanding
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.GroupTree
import lucuma.odb.sequence.isDayCalibration
import lucuma.odb.service.ObsExtract

/**
 * Evaluates the progress of groups toward satisfying their constraints.
 *
 * The rules:
 *
 *   - An inactive observation that isn't complete is left out of its group
 *     entirely, as is a complete one with no completion time (declared
 *     complete without ever having been observed), a daytime calibration and
 *     a subgroup left with no elements.  A group with no elements is empty.
 *     Staff may suspend an observation part way through, which leaves it out
 *     like an inactive one, but its group has still started: it is ongoing
 *     rather than pending.
 *     Daytime calibrations may be taken the following day, on another
 *     observing night, so a per-observation calibration group is just its
 *     science observation and any tellurics, all taken at night.
 *   - A complete observation whose QA state is usable or fail is rejected:
 *     its data isn't good enough, and it can't be observed again, so it never
 *     counts as complete and isn't checked against its group's constraints.
 *     One not yet assessed counts.  The calibrations in a per-observation
 *     calibration group go with their science observation, whatever their own
 *     QA states: they are left out when it is, and rejected when it is.  A
 *     subgroup with only rejected elements is itself rejected.
 *   - A group requires `minRequired` elements, or all of them when unset.
 *     An ordered group requires the first ones, in order.  A subgroup counts
 *     as one element, complete when its own status is complete.
 *   - "All of them" includes the rejected ones, so a group with no minimum
 *     and a rejected element can't complete: it is unschedulable until a
 *     minimum is set, which lets a replacement (a new observation) take the
 *     rejected one's place.  With a minimum, rejected elements are just left
 *     out.  A rejection can still break the group's time constraints, or the
 *     order of an ordered group whose later elements are already complete,
 *     since the completions left are checked without it, and a replacement
 *     observed later can't undo that.
 *   - Only the elements that count toward the group are checked.  An ordered
 *     group's elements after the first `minRequired` never count, but they
 *     must still complete in order like the rest.  An
 *     unordered group counts all of its elements until enough are complete,
 *     and from then on only the completions that met its minimum (in
 *     completion order): anything observed afterwards is extra, and can't
 *     make the group unschedulable, except under per-night limits (below).
 *     A complete subgroup is summarized for its parent (its start, nights and
 *     sites) from the elements that counted.
 *   - Zero intervals are treated as unset.  The intervals are checked between
 *     consecutive elements in completion order: the later one must start no
 *     earlier than the minimum interval after the earlier one completes, and
 *     complete no later than the maximum interval after it.  An element still
 *     executing is checked against the last completion as soon as it starts.
 *     (Suspending it leaves it out, and so takes back the violation.)
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
 *     minimum and may still take an extra.  Once a group with per-night limits
 *     has enough completions, the intervals check every completion its nights
 *     count, extras included, and an extra a short night needs is held to them
 *     like any other element: it must start no earlier than the minimum
 *     interval, and complete no later than the maximum, after the last.
 *   - Any violation makes the group unschedulable, including a deadline that
 *     `now` has passed: the maximum interval since the last completion
 *     (while the group is short of its count, or its night in progress of the
 *     per-night minimum), or the end of the completions' night with elements
 *     still to observe.  A night that ends before the minimum interval has
 *     passed is missed already.
 *     A night in progress short of the minimum is settled once too few
 *     elements remain to make it up.
 *   - Each violation is located (`GroupConstraintViolation`): the element
 *     out of order, the element an interval is measured from, or the night.
 *     An active waiver of a violation accepts it, and it no longer counts.
 *     Waiving an interval measured from the last completion also lifts that
 *     limit on the next window.  Waiving a night short of the per-night
 *     minimum accepts it, so the group no longer waits for an extra on it.
 *     For a same-night group, the completions on a waived night count but are
 *     exempt, and the rest must still share one night that isn't waived.
 *     The waivers that accepted a violation are reported with the progress.
 */
object GroupProgressEvaluator:

  /**
   * What the evaluator needs to know about an observation.  `calibrationRole`
   * is its role, if it is a calibration.  `science` is, for a per-observation
   * calibration, the science observation it goes with, as
   * `ObservationValidationInfo.Statements.AssociatedScienceObservation`
   * associates them.  `inactive` is true when a user set it inactive (or
   * declined it, for a telluric) before it started, or staff have suspended
   * its execution (or its science observation's, for a per-observation
   * calibration).  `qaState` is its QA state, if assessed (or overridden by
   * staff); it matters only once the observation is complete at a known
   * time, and `GroupProgressService` reads it only then.  `startTime` is when
   * it started executing, once it has started.  `completionTime` is when it
   * finished, once it is complete, and is unknown only for an observation
   * declared complete without ever having been observed.  (For a visitor
   * instrument, which records only slews, both come from its time charge; see
   * `GroupProgressService.Statements.SelectFacts`.)  `site` is where it was or
   * would be observed, if known.
   */
  final case class ObservationFacts(
    executionState:  ExecutionState,
    calibrationRole: Option[CalibrationRole],
    science:         Option[Observation.Id],
    inactive:        Boolean,
    qaState:         Option[ObservationQaState],
    startTime:       Option[Timestamp],
    completionTime:  Option[Timestamp],
    site:            Option[Site]
  ):
    def isStarted: Boolean =
      executionState match
        case ExecutionState.NotDefined | ExecutionState.NotStarted => false
        case _                                                     => true

    def isComplete: Boolean =
      executionState match
        case ExecutionState.Completed | ExecutionState.DeclaredComplete => true
        case _                                                          => false

    // Complete, but with data that isn't good enough.
    def isRejected: Boolean =
      isComplete && qaState.exists(_ =!= ObservationQaState.Pass)

  object ObservationFacts:
    val NotStarted: ObservationFacts =
      ObservationFacts(ExecutionState.NotStarted, None, None, false, None, None, None, None)

  /**
   * Evaluates every group in each of the trees, which should hold no deleted
   * elements.  An observation missing from `facts` is taken as not started.
   * `waivers` are each group's waivers; revoked ones are ignored.
   */
  def evaluate(
    trees:   List[GroupTree.Branch],
    facts:   Map[Observation.Id, ObservationFacts],
    waivers: Map[Group.Id, List[GroupConstraintWaiver]],
    now:     Timestamp
  ): Map[Group.Id, GroupProgress] =
    trees.flatMap(evaluateBranch(_, facts, waivers, now)._2).toMap

  /**
   * The standing of every group in `trees`, each a top-level group with all
   * of its subgroups, given their progress (from `evaluate`).  A group's
   * standing is decided by the outermost group that is complete or
   * unschedulable among the group itself and the groups containing it.  If
   * there is none the group is viable.  A top-level group, which no group
   * contains, is decided by its own progress alone.  An observation stands as
   * its group does.
   */
  def standings(
    trees:    List[GroupTree.Branch],
    progress: Map[Group.Id, GroupProgress]
  ): Map[Group.Id, GroupStanding] =
    def own(b: GroupTree.Branch): GroupStanding =
      progress.get(b.groupId) match
        case Some(_: GroupProgress.Complete)      => GroupStanding.Complete(b.groupId)
        case Some(_: GroupProgress.Unschedulable) => GroupStanding.Unschedulable(b.groupId)
        case _                                    => GroupStanding.Viable

    def go(b: GroupTree.Branch, outer: GroupStanding): List[(Group.Id, GroupStanding)] =
      val standing = outer match
        case GroupStanding.Viable => own(b)
        case decided              => decided
      (b.groupId -> standing) :: b.children.flatMap:
        case c: GroupTree.Branch => go(c, standing)
        case _: GroupTree.Leaf   => Nil

    trees.flatMap(go(_, GroupStanding.Viable)).toMap

  // A group element as its parent sees it: an observation, or a subgroup
  // summarized, identified by `id`.  `excluded` elements are left out of the group entirely.
  // `rejected` ones never count, but a group with no minimum still requires
  // them.  `completion` is when it completed, once it has.  `started` is
  // whether it has started its group, left out or not.  `nights` are the
  // nights of the completions it counts, each at the site of its completion,
  // and `sites` the sites of the observations it holds.
  private final case class Element(
    id:            Either[Group.Id, Observation.Id],
    excluded:      Boolean,
    rejected:      Boolean,
    completion:    Option[Timestamp],
    unschedulable: Boolean,
    started:       Boolean,
    startTime:     Option[Timestamp],
    nights:        Set[ObservingNight],
    sites:         Set[Site]
  ):
    def complete: Boolean =
      completion.isDefined

    def isRemaining: Boolean =
      !complete && !unschedulable

  // `science` is the element of the leaf's science observation, which a
  // per-observation calibration goes with.
  private def leafElement(oid: Observation.Id, leaf: ObservationFacts, science: Option[Element]): Element =
    val complete = leaf.isComplete
    val started  = leaf.isStarted
    val time     = Option.when(complete)(leaf.completionTime).flatten
    // Left out when a daytime calibration, inactive and incomplete, or
    // complete with no known time.  Otherwise rejected, if it is.  A
    // per-observation calibration is left out or rejected with its science
    // observation, whatever its own QA state.
    val perObs   = leaf.calibrationRole.exists(ObsExtract.PerObservationCalibrationRoles.contains)
    val daytime  = leaf.calibrationRole.isDayCalibration
    val excluded = daytime || (if complete then time.isEmpty else leaf.inactive) || (perObs && science.exists(_.excluded))
    val rejected = !excluded && (if perObs then science.exists(_.rejected) else leaf.isRejected)
    // It starts its group even when left out for having been suspended part
    // way through, but not as a daytime calibration or when complete
    // without ever having been observed.
    val startsGroup = started && !daytime && (!complete || time.isDefined)

    Element(
      id            = oid.asRight,
      excluded      = excluded,
      rejected      = rejected,
      completion    = time,
      unschedulable = false,
      started       = startsGroup,
      startTime     = Option.when(started)(leaf.startTime).flatten,
      nights        = (for t <- time; s <- leaf.site yield night(s, t).atSite(s)).toSet,
      sites         = if excluded then Set.empty else leaf.site.toSet
    )

  private def night(site: Site, time: Timestamp): LocalObservingNight =
    LocalObservingNight.fromSiteAndInstant(site, time.toInstant)

  private def nightEnd(n: LocalObservingNight, site: Site): Timestamp =
    Timestamp.fromInstantTruncatedAndBounded(n.atSite(site).end)

  // A group's observations per night, given its limits: `min` (1 when unset)
  // and `max`.  `completions` are those that count toward the group, in order,
  // and `sites` the sites of the elements that count.
  //
  // The limits apply only at a single site, where each completion falls on one
  // night.  Every completion of the group's elements counts toward its night,
  // extras included, until the group completes, and then those on the night it
  // completes.  The night in progress, if any, is that of the last of these,
  // until it ends.  `minimumWaived` says whether a waiver accepts a night's
  // shortfall.
  private final class ObservationsPerNight(
    min:           Int,
    max:           Option[Int],
    viable:        List[Element],
    completions:   List[(Timestamp, Element)],
    sites:         Set[Site],
    countMet:      Boolean,
    now:           Timestamp,
    minimumWaived: ObservingNight => Boolean
  ):

    // The completions counted toward their nights, in order.
    val counted: List[(Timestamp, Element)] =
      val all = viable.mapFilter(e => e.completion.tupleRight(e))
      (if !countMet then all
      else
        (for
          s <- Option.when(sites.size == 1)(sites.head)
          t <- completions.lastOption.map(_._1)
          e  = nightEnd(night(s, t), s)
        yield all.filter(_._1 < e)).getOrElse(completions)).sortBy(_._1)

    private val allSites: Set[Site] =
      if countMet then sites ++ counted.flatMap(_._2.sites)
      else viable.flatMap(_.sites).toSet

    val site: Option[Site] =
      Option.when(allSites.size == 1)(allSites.head)

    private val countsByNight: Map[LocalObservingNight, Int] =
      site.fold(Map.empty): s =>
        counted.groupMapReduce((t, _) => night(s, t))(_ => 1)(_ + _)

    private val openNight: Option[(LocalObservingNight, Timestamp)] =
      for
        s <- site
        t <- counted.map(_._1).maxOption
        n  = night(s, t)
        e  = nightEnd(n, s)
        if now < e
      yield (n, e)

    private val openCount: Int =
      openNight.fold(0)((n, _) => countsByNight.getOrElse(n, 0))

    // The elements still to observe, extras included.
    private val available: Int =
      viable.count(_.isRemaining)

    // Its observations are at more than one site, so its nights are ambiguous.
    val multipleSites: Boolean =
      allSites.size > 1

    // The night in progress is short of the minimum, and so must take more,
    // unless a waiver accepts its shortfall.
    val openNightShort: Boolean =
      openNight.exists: (n, _) =>
        openCount < min && !site.exists(s => minimumWaived(n.atSite(s)))

    // The nights short of the minimum: those that have ended, and the night in
    // progress once it can't be made up (the next element can't start, per the
    // minimum interval, before its end, or too few elements remain).
    def minimumViolations(nextStart: Option[Timestamp]): List[ObservingNight] =
      site.toList.flatMap: s =>
        countsByNight.toList.sortBy(_._1).collect:
          case (n, c) if c < min && openNight.forall((o, e) => o =!= n || nextStart.exists(_ >= e) || available < min - c) =>
            n.atSite(s)

    // The nights with more than the maximum.
    val maximumViolations: List[ObservingNight] =
      (site, max).tupled.toList.flatMap: (s, m) =>
        countsByNight.toList.sortBy(_._1).collect:
          case (n, c) if c > m => n.atSite(s)

    // A night in progress short of the minimum must take the next element, so
    // it limits the window's end, and one at the maximum can't, so the window
    // starts when it ends.
    val windowEnd: Option[Timestamp] =
      openNight.collect { case (_, e) if openNightShort => e }

    val windowStart: Option[Timestamp] =
      openNight.collect { case (_, e) if max.exists(openCount >= _) => e }

  private def evaluateBranch(
    branch:  GroupTree.Branch,
    facts:   Map[Observation.Id, ObservationFacts],
    waivers: Map[Group.Id, List[GroupConstraintWaiver]],
    now:     Timestamp
  ): (Element, Map[Group.Id, GroupProgress]) =

    def leafFacts(oid: Observation.Id): ObservationFacts =
      facts.getOrElse(oid, ObservationFacts.NotStarted)

    def leaf(oid: Observation.Id): Element =
      val f = leafFacts(oid)
      leafElement(oid, f, f.science.map(sci => leafElement(sci, leafFacts(sci), None)))

    val evaluated: List[(Element, Map[Group.Id, GroupProgress])] =
      branch.children.map:
        case GroupTree.Leaf(oid) => (leaf(oid), Map.empty)
        case b: GroupTree.Branch => evaluateBranch(b, facts, waivers, now)

    val nested: Map[Group.Id, GroupProgress] =
      evaluated.flatMap(_._2).toMap

    val elements: List[Element] =
      evaluated.map(_._1).filterNot(_.excluded)

    // Whether anything in the group has started it, left out or not.  An
    // empty group still tells its parent so, as when its observations were
    // all suspended part way through.
    val started: Boolean =
      evaluated.exists(_._1.started)

    val active: List[GroupConstraintWaiver] =
      waivers.getOrElse(branch.groupId, Nil).filter(_.isActive)

    val (progress, element) =
      NonEmptyList.fromList(elements).fold(
        (GroupProgress.Empty, emptyElement(branch.groupId, started))
      )(evaluateElements(branch, _, started, active, now))

    (element, nested + (branch.groupId -> progress))

  // An empty group, which its parent leaves out (though it may have started
  // the parent).
  private def emptyElement(id: Group.Id, started: Boolean): Element =
    Element(
      id            = id.asLeft,
      excluded      = true,
      rejected      = false,
      completion    = None,
      unschedulable = false,
      started       = started,
      startTime     = None,
      nights        = Set.empty,
      sites         = Set.empty
    )

  // `all` are the elements that aren't left out, rejected ones included.
  // `started` is whether anything in the group has started it.  `waivers` are
  // the group's active waivers.
  private def evaluateElements(
    branch:  GroupTree.Branch,
    all:     NonEmptyList[Element],
    started: Boolean,
    waivers: List[GroupConstraintWaiver],
    now:     Timestamp
  ): (GroupProgress, Element) =

    import GroupConstraintViolation.Unwaivable.*
    import GroupConstraintViolation.Waivable
    import GroupConstraintViolation.Waivable.*

    // The waiver accepting each violation it names.
    val waiverFor: Map[Waivable, GroupConstraintWaiver] =
      waivers.map(w => w.violation -> w).toMap

    def isWaived(v: GroupConstraintViolation): Boolean =
      v match
        case w: Waivable => waiverFor.contains(w)
        case _           => false

    // The elements that may yet count.
    val viable: List[Element] =
      all.filterNot(_.rejected)

    val minInterval = branch.minInterval.filterNot(_.isZero)
    val maxInterval = branch.maxInterval.filterNot(_.isZero)
    // The minimum, or all of the elements when it is unset (or zero, which the
    // database forbids and would mean unset), rejected ones included, so that
    // a group with no minimum can't complete without them.  Either way, at
    // least one.
    val required: PosInt =
      branch.minRequired.flatMap(n => PosInt.from(n.value.toInt).toOption).getOrElse(PosInt.unsafeFrom(all.length))

    // The elements that count toward the group, which everything below
    // considers.  An ordered group requires its first `required` elements, so
    // later ones never count.  An unordered group counts all of them until
    // enough are complete, and then only the earliest completions that met
    // the minimum.
    val elements: List[Element] =
      if branch.ordered then viable.take(required.value)
      else
        val done = viable.filter(_.complete)
        if done.size < required.value then viable
        else done.sortBy(_.completion).take(required.value)

    // The completions, in order.
    val completions: List[(Timestamp, Element)] =
      elements.mapFilter(e => e.completion.tupleRight(e)).sortBy(_._1)

    val completed: List[Element]  = completions.map(_._2)
    val timed: List[Timestamp]    = completions.map(_._1)
    val remaining: List[Element]  = elements.filter(_.isRemaining)

    val nights: Set[ObservingNight] =
      completed.flatMap(_.nights).toSet

    val sites: Set[Site] =
      elements.flatMap(_.sites).toSet

    // The completions' nights, in the order they fall (by date, which
    // ObservingNight's own order puts after the site).  For a same-night
    // group, those whose completions a waiver accepts are exempt, and the
    // rest must be a single night.  Nights at different sites are different
    // nights.
    val sameNights: List[ObservingNight] =
      nights.toList.sortBy(n => (n.toLocalObservingNight, n.site))

    val (sameNightsWaived, sameNightsHeld) =
      sameNights.partition(n => isWaived(SameNight(n)))

    // The end of the night the same-night group is held to, once it has
    // completions on a single night that isn't waived.  A group spanning
    // sites is a violation of its own, so the group has one site, known from
    // its completions even when the observations still to do have no
    // instrument yet.
    val sameNightEnd: Option[Timestamp] =
      sameNightsHeld match
        case List(n) if branch.sameNight && sites.size == 1 => Some(nightEnd(n.toLocalObservingNight, n.site))
        case _                                              => None

    val completedCount: NonNegInt = NonNegInt.unsafeFrom(completed.size)

    // Whether enough of the elements that count are complete (for an ordered
    // group, those are its first `required`, so all of them).
    val countMet: Boolean =
      completed.size >= required.value

    // Whether `later` started before the minimum interval had passed since a
    // completion at `t`.
    def startedTooSoon(t: Timestamp, later: Element): Boolean =
      (for
        min <- minInterval
        s   <- later.startTime
      yield s < (t +| min)).getOrElse(false)

    // The elements completed before an earlier one: either the earlier one is
    // still incomplete or it completed later.  Every element that may count
    // is held to the order, including an ordered group's elements after its
    // first `required`, which never count.
    val outOfOrder: List[Element] =
      viable.zipWithIndex.collect:
        case (later, j) if later.completion.exists(tl => viable.take(j).exists(_.completion.forall(_ > tl))) => later

    // Observations per night, for a group with either limit.
    val perNight: Option[ObservationsPerNight] =
      Option.when(branch.minObsPerNight.isDefined || branch.maxObsPerNight.isDefined)(
        ObservationsPerNight(
          min           = branch.minObsPerNight.fold(1)(_.value.toInt),
          max           = branch.maxObsPerNight.map(_.value.toInt),
          viable        = viable,
          completions   = completions,
          sites         = sites,
          countMet      = countMet,
          now           = now,
          minimumWaived = n => isWaived(ObservationsPerNightMinimum(n))
        )
      )

    // A night in progress short of the per-night minimum, which must take an
    // extra even though the group has enough completions.
    val nightShort: Boolean =
      perNight.exists(_.openNightShort)

    // The completions the intervals check, in order: those that count toward
    // the group and, once it has enough, every one its nights count (extras
    // included) when it has per-night limits.
    val paced: List[(Timestamp, Element)] =
      if countMet then perNight.fold(completions)(_.counted) else completions

    // Each paced completion but the first, with the one before it.
    val consecutive: List[((Timestamp, Element), (Timestamp, Element))] =
      paced.zip(paced.drop(1))

    // The elements still to observe that the intervals check: those that
    // count toward the group, or once it has enough, any that could be the
    // extra a short night in progress needs.
    val unpaced: List[Element] =
      if !countMet then remaining
      else if nightShort then viable.filter(_.isRemaining)
      else Nil

    // The last paced completion, from which the next window is measured.  A
    // waiver of an interval measured from it lifts that limit.
    val last: Option[(Timestamp, Element)] =
      paced.lastOption

    val lastMinimum: Option[TimeSpan] =
      minInterval.filterNot(_ => last.exists((_, e) => isWaived(MinimumInterval(e.id))))

    val lastMaximum: Option[TimeSpan] =
      maxInterval.filterNot(_ => last.exists((_, e) => isWaived(MaximumInterval(e.id))))

    // The earliest the next element may start, per the minimum interval.
    val nextStart: Option[Timestamp] =
      last.map((t, _) => lastMinimum.fold(t)(t +| _))

    // The window ends at `Timestamp.Max` when nothing limits its end.  (There
    // is no window that would end before it starts, rather than one with its
    // ends swapped, but then the group is unschedulable anyway: the minimum
    // interval never exceeds the maximum, and a night that ends too soon is a
    // violation below.)
    val nextWindow: Option[TimestampInterval] =
      for
        (t, _) <- last
        start0 <- nextStart
        if minInterval.isDefined || maxInterval.isDefined || branch.sameNight || perNight.isDefined
        start   = (start0 :: perNight.flatMap(_.windowStart).toList).max
        end     = (lastMaximum.map(t +| _).toList ++ sameNightEnd.toList ++ perNight.flatMap(_.windowEnd).toList).minOption.getOrElse(Timestamp.Max)
        if start <= end
      yield TimestampInterval.between(start, end)

    // The violations found, waived or not.  An unschedulable subgroup sinks
    // the group only if the group needs it.  Each completion is checked
    // against the one before it: it must have started after the minimum
    // interval and completed within the maximum.  An element still executing
    // comes after the last completion, and starting too soon can't be undone,
    // so it is checked as soon as it starts.  The deadlines (the maximum
    // interval, and the end of a same-night group's night, which the minimum
    // interval may already reach) matter only while the group is short of
    // completion.  For a same-night group, every night held to the rule but
    // the last is a violation, and so is the last once it ends with elements
    // still to do; a waived night is listed too, as accepted.
    val found: List[GroupConstraintViolation] =
      val unschedulable = elements.filter(_.unschedulable)
      val subgroupNeeded =
        if branch.ordered then unschedulable.nonEmpty
        else elements.count(!_.unschedulable) < required.value && unschedulable.nonEmpty
      List(
        if subgroupNeeded then unschedulable.flatMap(_.id.left.toOption).map(SubgroupUnschedulable(_)) else Nil,
        if elements.size < required.value then List(InsufficientElements) else Nil,
        if branch.ordered then outOfOrder.map(e => Order(e.id)) else Nil,
        consecutive.collect { case ((t0, e0), (_, e1)) if startedTooSoon(t0, e1) => MinimumInterval(e0.id) } ++
          last.toList.collect { case (t, e) if unpaced.exists(startedTooSoon(t, _)) => MinimumInterval(e.id) },
        maxInterval.toList.flatMap: max =>
          consecutive.collect { case ((t0, e0), (t1, _)) if TimeSpan.between(t0, t1) > max => MaximumInterval(e0.id) } ++
            last.toList.collect { case (t, e) if (!countMet || nightShort) && now > (t +| max) => MaximumInterval(e.id) },
        if branch.sameNight && sites.size > 1 then List(SameNightMultipleSites) else Nil,
        if branch.sameNight then
          sameNightsHeld.dropRight(1).map(SameNight(_)) ++
            sameNightsHeld.lastOption.filter(_ => !countMet && sameNightEnd.exists(end => now >= end || nextStart.exists(_ >= end))).map(SameNight(_)) ++
            sameNightsWaived.map(SameNight(_))
        else Nil,
        if perNight.exists(_.multipleSites) then List(ObservationsPerNightMultipleSites) else Nil,
        perNight.toList.flatMap(_.minimumViolations(nextStart)).map(ObservationsPerNightMinimum(_)),
        perNight.toList.flatMap(_.maximumViolations).map(ObservationsPerNightMaximum(_))
      ).flatten.distinct

    // Those no waiver accepts, and the waivers that accept the rest.
    val violations: List[GroupConstraintViolation] =
      found.filterNot(isWaived)

    val waived: List[GroupConstraintWaiver] =
      found.collect { case w: Waivable => w }.flatMap(waiverFor.get)

    val progress: GroupProgress =
      NonEmptyList.fromList(violations) match
        case Some(vs)         => GroupProgress.Unschedulable(completedCount, required, vs, waived)
        case None if countMet && !nightShort => GroupProgress.Complete(completedCount, required, waived)
        case None if started  => GroupProgress.Ongoing(completedCount, required, nextWindow, waived)
        case None             => GroupProgress.Pending(required)

    val status: GroupStatus = progress.status

    // Its start, nights and sites, for its parent, are those of the elements
    // that counted, and it completed with the last of the completions that
    // counted.  (Complete, it has at least one.)  With only rejected elements,
    // it is rejected itself.
    val element = Element(
      id            = branch.groupId.asLeft,
      excluded      = false,
      rejected      = viable.isEmpty,
      completion    = Option.when(status === GroupStatus.Complete)(timed.lastOption).flatten,
      unschedulable = status === GroupStatus.Unschedulable,
      started       = started,
      startTime     = elements.flatMap(_.startTime).minOption,
      nights        = nights,
      sites         = sites
    )

    (progress, element)
