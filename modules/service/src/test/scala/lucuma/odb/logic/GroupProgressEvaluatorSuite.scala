// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.NonEmptySet
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupConstraintViolation.*
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.group.GroupProgress
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.GroupTree
import munit.FunSuite

import java.time.Instant

class GroupProgressEvaluatorSuite extends FunSuite:

  import GroupProgressEvaluator.ObservationFacts

  def oid(n: Long): Observation.Id = Observation.Id.fromLong(n).get
  def gid(n: Long): Group.Id       = Group.Id.fromLong(n).get

  def ts(s: String): Timestamp     = Timestamp.unsafeFromInstant(Instant.parse(s))
  def hours(h: Int): TimeSpan      = TimeSpan.fromHours(h).get
  def minutes(m: Int): TimeSpan    = TimeSpan.fromMinutes(m).get

  // The night of 15-16 October 2026 at both sites.
  val t0: Timestamp = ts("2026-10-16T05:00:00Z") // 19:00 HST, 02:00 CLT

  // A tree element along with the facts of the observations in it.
  case class Node(child: GroupTree.Child, facts: Map[Observation.Id, ObservationFacts]):
    def branch: GroupTree.Branch =
      child match
        case b: GroupTree.Branch => b
        case l: GroupTree.Leaf   => fail(s"Expected a group, not $l")

  def obs(n: Long, facts: ObservationFacts): Node =
    Node(GroupTree.Leaf(oid(n)), Map(oid(n) -> facts))

  def leaf(
    n:        Long,
    state:    ExecutionState    = ExecutionState.NotStarted,
    time:     Option[Timestamp] = None,
    site:     Site              = Site.GN,
    inactive: Boolean           = false,
    start:    Option[Timestamp] = None
  ): Node =
    obs(n, ObservationFacts(state, inactive, start, time, Some(site)))

  // A completed observation, by default started and completed at `t`.
  def done(n: Long, t: Timestamp, site: Site = Site.GN, start: Option[Timestamp] = None): Node =
    leaf(n, ExecutionState.Completed, Some(t), site, start = start.orElse(Some(t)))

  def branch(
    n:         Long,
    children:  List[Node],
    min:       Option[Int]      = None,
    ordered:   Boolean          = false,
    minI:      Option[TimeSpan] = None,
    maxI:      Option[TimeSpan] = None,
    sameNight: Boolean          = false,
    minN:      Option[Int]      = None,
    maxN:      Option[Int]      = None
  ): Node =
    Node(
      GroupTree.Branch(
        groupId          = gid(n),
        minRequired      = min.map(m => NonNegShort.unsafeFrom(m.toShort)),
        ordered          = ordered,
        children         = children.map(_.child),
        name             = None,
        description      = None,
        minInterval      = minI,
        maxInterval      = maxI,
        system           = false,
        calibrationRoles = Nil,
        sameNight        = sameNight,
        minObsPerNight   = minN.map(m => PosShort.unsafeFrom(m.toShort)),
        maxObsPerNight   = maxN.map(m => PosShort.unsafeFrom(m.toShort))
      ),
      children.flatMap(_.facts).toMap
    )

  def evalAll(n: Node, now: Timestamp = t0): Map[Group.Id, GroupProgress] =
    GroupProgressEvaluator.evaluate(List(n.branch), n.facts, now)

  def eval(n: Node, now: Timestamp = t0): GroupProgress =
    evalAll(n, now)(n.branch.groupId)

  def pos(n: Int): PosInt   = PosInt.unsafeFrom(n)
  def nn(n: Int): NonNegInt = NonNegInt.unsafeFrom(n)

  def pending(required: Int): GroupProgress =
    GroupProgress.Pending(pos(required))

  def ongoing(completed: Int, required: Int, nextWindow: Option[TimestampInterval] = None): GroupProgress =
    GroupProgress.Ongoing(nn(completed), pos(required), nextWindow)

  def complete(completed: Int, required: Int): GroupProgress =
    GroupProgress.Complete(nn(completed), pos(required))

  def unschedulable(completed: Int, required: Int, v: GroupConstraintViolation, vs: GroupConstraintViolation*): GroupProgress =
    GroupProgress.Unschedulable(nn(completed), pos(required), NonEmptySet.of(v, vs*))

  extension (p: GroupProgress)
    def violationList: List[GroupConstraintViolation] =
      p match
        case GroupProgress.Unschedulable(_, _, v) => v.toSortedSet.toList
        case _                                    => Nil
    def window: Option[TimestampInterval] =
      p match
        case GroupProgress.Ongoing(_, _, w) => w
        case _                              => None

  // Counting

  test("nothing started: pending"):
    assertEquals(eval(branch(1, List(leaf(1), leaf(2)))), pending(2))

  test("one element executing: ongoing"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Ongoing), leaf(2)))),
      ongoing(0, 2)
    )

  test("declared ongoing counts as started, not complete"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.DeclaredOngoing)))),
      ongoing(0, 1)
    )

  test("n of m met: complete"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2), done(3, t0 +| hours(1))), min = Some(2))),
      complete(2, 2)
    )

  test("complete while another element is still executing"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing)), min = Some(1))).status,
      GroupStatus.Complete
    )

  test("declared complete counts as complete"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.DeclaredComplete, Some(t0))))).status,
      GroupStatus.Complete
    )

  test("declared complete without ever having been observed is left out"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, ExecutionState.DeclaredComplete)))),
      complete(1, 1)
    )
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.DeclaredComplete)))),
      GroupProgress.Empty
    )

  test("inactive observations don't count toward 'all required'"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, inactive = true), done(3, t0)))),
      complete(2, 2)
    )

  test("an inactive observation that is complete still counts"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Completed, Some(t0), inactive = true), leaf(2)))),
      ongoing(1, 2)
    )

  test("too few viable elements: insufficient elements"):
    assertEquals(
      eval(branch(1, List(leaf(1), leaf(2, inactive = true), leaf(3)), min = Some(3))),
      unschedulable(0, 3, InsufficientElements)
    )

  test("a group with nothing viable is empty, and its parent leaves it out"):
    val empty  = branch(2, List(leaf(2, inactive = true), leaf(3, inactive = true)))
    val parent = branch(1, List(done(1, t0), empty))
    val all    = evalAll(parent, t0)
    assertEquals(all(gid(2)), GroupProgress.Empty)
    assertEquals(all(gid(1)), complete(1, 1))

  test("a new, empty subgroup doesn't complete a one-of-n parent"):
    val parent = branch(1, List(branch(2, Nil), leaf(2)), min = Some(1))
    val all    = evalAll(parent)
    assertEquals(all(gid(2)), GroupProgress.Empty)
    assertEquals(all(gid(1)), pending(1))

  // Order

  test("ordered: pending, then ongoing as the first elements complete"):
    assertEquals(
      eval(branch(1, List(leaf(1), leaf(2)), ordered = true)),
      pending(2)
    )
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), ordered = true)),
      ongoing(1, 2)
    )

  test("ordered: a later element completed before an earlier incomplete one"):
    assertEquals(
      eval(branch(1, List(leaf(1), done(2, t0)), ordered = true)),
      unschedulable(1, 2, Order)
    )

  test("ordered: completion times out of order"):
    assertEquals(
      eval(branch(1, List(done(1, t0 +| hours(1)), done(2, t0)), ordered = true)).violationList,
      List(Order)
    )

  test("ordered n of m: requires the first n"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3)), min = Some(2), ordered = true)),
      complete(2, 2)
    )
    // The third element never counts, so completing it out of order is no
    // violation.
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2), done(3, t0 +| hours(1))), min = Some(2), ordered = true)),
      ongoing(1, 2)
    )

  test("ordered n of m: an element after the first n completing first doesn't matter"):
    assertEquals(
      eval(branch(1, List(done(1, t0 +| hours(1)), done(2, t0 +| hours(2)), done(3, t0)), min = Some(2), ordered = true)),
      complete(2, 2)
    )

  // Extra elements

  test("unordered: an element started too soon after the group completed is extra"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing, start = Some(t0 +| minutes(30)))), min = Some(1), minI = Some(hours(1)))
    assertEquals(eval(g, now = t0 +| hours(1)), complete(1, 1))

  test("unordered: a completion after the group completed is extra, however late"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(3))), min = Some(1), maxI = Some(hours(1)))
    assertEquals(eval(g, now = t0 +| hours(4)), complete(1, 1))

  test("unordered: only the completions that met the minimum are counted"):
    assertEquals(eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1))), min = Some(1))), complete(1, 1))

  test("same night: an observation on another night after the group completed is extra"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, ts("2026-10-17T05:00:00Z"))), min = Some(2), sameNight = true)
    assertEquals(eval(g), complete(2, 2))

  test("a violation among the completions that met the minimum still counts"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| minutes(30)), leaf(3)), min = Some(2), minI = Some(hours(1)))
    assertEquals(eval(g).violationList, List(MinimumInterval))

  test("a complete subgroup's start, for its parent, is that of the elements that counted"):
    // Observation 3 started only 10 minutes after observation 1 completed, but
    // it completed after observation 2 had already completed the subgroup.
    val sub    = branch(2, List(done(2, t0 +| hours(2), start = Some(t0 +| minutes(90))), done(3, t0 +| hours(3), start = Some(t0 +| minutes(10)))), min = Some(1))
    val parent = branch(1, List(done(1, t0), sub), minI = Some(hours(1)))
    assertEquals(eval(parent, now = t0 +| hours(4)), complete(2, 2))

  // Intervals

  test("completions closer than the minimum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| minutes(30))), minI = Some(hours(1)))).violationList,
      List(MinimumInterval)
    )

  test("started before the minimum interval, though completed after it"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| minutes(90), start = Some(t0 +| minutes(30)))), minI = Some(hours(1)))
    assertEquals(eval(g).violationList, List(MinimumInterval))

  test("started after the minimum interval"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(2), start = Some(t0 +| hours(1)))), minI = Some(hours(1)))
    assertEquals(eval(g), complete(2, 2))

  test("an element still executing that started too soon is unschedulable now"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing, start = Some(t0 +| minutes(30)))), minI = Some(hours(1)))
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      unschedulable(1, 2, MinimumInterval)
    )

  test("an element still executing that started after the minimum interval"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing, start = Some(t0 +| hours(1)))), minI = Some(hours(1)))
    assertEquals(
      eval(g, now = t0 +| hours(2)),
      ongoing(1, 2, nextWindow = Some(TimestampInterval.from(t0 +| hours(1))))
    )

  test("a subgroup starts when its first observation starts"):
    // The subgroup completes two hours after observation 1, but its first
    // observation started only 30 minutes after.
    val sub    = branch(2, List(done(2, t0 +| hours(1), start = Some(t0 +| minutes(30))), done(3, t0 +| hours(2))))
    val parent = branch(1, List(done(1, t0), sub), minI = Some(hours(1)))
    assertEquals(eval(parent).violationList, List(MinimumInterval))

  test("completions further apart than the maximum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(2))), maxI = Some(hours(1)))).violationList,
      List(MaximumInterval)
    )

  test("intervals compare consecutive completions in completion order"):
    val g = branch(1, List(done(1, t0 +| hours(4)), done(2, t0), done(3, t0 +| hours(2))), minI = Some(hours(1)), maxI = Some(hours(3)))
    assertEquals(eval(g), complete(3, 3))

  test("maximum interval deadline missed: unschedulable"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), maxI = Some(hours(1))), now = t0 +| hours(2)),
      unschedulable(1, 2, MaximumInterval)
    )

  test("finishing after the maximum interval is a violation even if started within it"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(2))), maxI = Some(hours(1)))).status,
      GroupStatus.Unschedulable
    )

  test("next window: from the minimum to the maximum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(1)), maxI = Some(hours(3))), now = t0 +| minutes(30)),
      ongoing(1, 2, nextWindow = Some(TimestampInterval.between(t0 +| hours(1), t0 +| hours(3))))
    )

  test("next window: open-ended with only a minimum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(1)))).window,
      Some(TimestampInterval.from(t0 +| hours(1)))
    )

  test("no window without interval or night constraints"):
    assertEquals(eval(branch(1, List(done(1, t0), leaf(2)))).window, None)

  test("zero intervals are unset"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(48)), leaf(3)), minI = Some(TimeSpan.Zero), maxI = Some(TimeSpan.Zero))
    assertEquals(eval(g, now = t0 +| hours(100)), ongoing(2, 3))

  test("declared complete without a time is neither counted nor interval-checked"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.DeclaredComplete)), maxI = Some(hours(1)))
    assertEquals(eval(g, now = t0 +| hours(5)), complete(1, 1))

  test("declared complete with a time is checked like any other completion"):
    val declared = leaf(2, ExecutionState.DeclaredComplete, Some(t0 +| hours(2)), start = Some(t0 +| hours(2)))
    assertEquals(
      eval(branch(1, List(done(1, t0), declared), maxI = Some(hours(1)))),
      unschedulable(2, 2, MaximumInterval)
    )

  test("a subgroup's minimum isn't met by an untimed completion"):
    // Observation 2 is declared complete without ever executing, so the
    // subgroup's minimum of two is met only by observation 4, too late.
    val sub    = branch(2, List(leaf(2, ExecutionState.DeclaredComplete), done(3, t0 +| minutes(30)), done(4, t0 +| hours(5))), min = Some(2))
    val parent = branch(1, List(done(1, t0), sub), maxI = Some(hours(1)))
    assertEquals(eval(parent, now = t0 +| hours(6)), unschedulable(2, 2, MaximumInterval))

  test("a subgroup of only untimed completions is left out"):
    val sub    = branch(2, List(leaf(2, ExecutionState.DeclaredComplete), leaf(3, ExecutionState.DeclaredComplete)))
    val parent = branch(1, List(done(1, t0), sub), maxI = Some(hours(1)))
    assertEquals(evalAll(parent, now = t0 +| hours(6))(gid(2)), GroupProgress.Empty)
    assertEquals(eval(parent, now = t0 +| hours(6)), complete(1, 1))

  // Same night

  test("same night: completions on the same night"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1))), sameNight = true)
    assertEquals(eval(g), complete(2, 2))

  test("same night: completions on different nights"):
    // 19:00 HST on 16 October is in the night of 16-17 October.
    val g = branch(1, List(done(1, t0), done(2, ts("2026-10-17T05:00:00Z"))), sameNight = true)
    assertEquals(eval(g).violationList, List(SameNight))

  test("same night: the night ends at the group's site even with no instrument still to do"):
    // Observation 2 has no instrument, so no site of its own.
    val noSite = obs(2, ObservationFacts.NotStarted)
    val g      = branch(1, List(done(1, t0), noSite), sameNight = true)
    assertEquals(eval(g, now = t0 +| hours(1)).window, Some(TimestampInterval.between(t0, ts("2026-10-17T00:00:00Z"))))
    assertEquals(
      eval(g, now = ts("2026-10-17T01:00:00Z")),
      unschedulable(1, 2, SameNight)
    )

  test("same night: a completion without a time is left out"):
    val g = branch(1, List(done(1, t0), done(2, ts("2026-10-17T05:00:00Z")), leaf(3, ExecutionState.DeclaredComplete)), sameNight = true)
    assertEquals(eval(g), unschedulable(2, 2, SameNight))

  test("same night: observations at more than one site"):
    val g = branch(1, List(done(1, t0, Site.GN), done(2, t0 +| hours(1), Site.GS)), sameNight = true)
    assertEquals(eval(g), unschedulable(2, 2, SameNightMultipleSites))

  test("same night: an observation still to do at another site"):
    val g = branch(1, List(leaf(1, site = Site.GN), leaf(2, site = Site.GS)), sameNight = true)
    assertEquals(eval(g), unschedulable(0, 2, SameNightMultipleSites))

  test("same night: an inactive observation at another site doesn't count"):
    val g = branch(1, List(done(1, t0), leaf(2), leaf(3, site = Site.GS, inactive = true)), sameNight = true)
    assertEquals(eval(g).status, GroupStatus.Ongoing)

  test("same night: a subgroup's observations count toward its parent's sites"):
    val sub    = branch(2, List(leaf(2, site = Site.GS)))
    val parent = branch(1, List(done(1, t0, Site.GN), sub), sameNight = true)
    assertEquals(eval(parent).violationList, List(SameNightMultipleSites))

  test("same night: window ends with the night at the remaining site"):
    // The GN night of 15-16 October ends at 14:00 HST, 00:00 UT on the 17th.
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, site = Site.GN)), sameNight = true), now = t0 +| hours(1)).window,
      Some(TimestampInterval.between(t0, ts("2026-10-17T00:00:00Z")))
    )

  test("same night: the night ended with an element still to observe"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, site = Site.GN)), sameNight = true), now = ts("2026-10-17T01:00:00Z")),
      unschedulable(1, 2, SameNight)
    )

  test("same night: the window starts after the minimum interval"):
    val g = branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(2)), sameNight = true)
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      ongoing(1, 2, nextWindow = Some(TimestampInterval.between(t0 +| hours(2), ts("2026-10-17T00:00:00Z"))))
    )

  test("same night: the night ends before the minimum interval passes"):
    // The next observation couldn't start until 01:00 UT on the 17th, an hour
    // after the night ends, so the group is unschedulable before the night is
    // over.
    val g = branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(20)), sameNight = true)
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      unschedulable(1, 2, SameNight)
    )

  test("same night: the minimum interval ending as the night ends leaves no time"):
    val g = branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(19)), sameNight = true)
    assertEquals(eval(g, now = t0 +| hours(1)).status, GroupStatus.Unschedulable)

  test("same night: a complete group isn't sunk by the minimum interval"):
    val g = branch(1, List(done(1, t0), leaf(2)), min = Some(1), minI = Some(hours(20)), sameNight = true)
    assertEquals(eval(g, now = t0 +| hours(1)), complete(1, 1))

  // Observations per night

  // The GN night of 15-16 October ends at 00:00 UT on the 17th, and t1 falls in
  // the next one.
  val night0End: Timestamp = ts("2026-10-17T00:00:00Z")
  val t1: Timestamp        = ts("2026-10-17T05:00:00Z")

  test("per night: enough on one night"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1))), minN = Some(2))
    assertEquals(eval(g), complete(2, 2))

  test("per night: a night in progress short of the minimum ends the window with the night"):
    val g = branch(1, List(done(1, t0), leaf(2), leaf(3)), minN = Some(2))
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      ongoing(1, 3, nextWindow = Some(TimestampInterval.between(t0, night0End)))
    )

  test("per night: a night that ended short of the minimum"):
    val g = branch(1, List(done(1, t0), done(2, t1), done(3, t1 +| hours(1)), leaf(4)), minN = Some(2))
    assertEquals(eval(g, now = t1 +| hours(2)), unschedulable(3, 4, ObservationsPerNightMinimum))

  test("per night: a night short of the minimum once the minimum interval passes its end"):
    val g = branch(1, List(done(1, t0), leaf(2), leaf(3)), minI = Some(hours(20)), minN = Some(2))
    assertEquals(eval(g, now = t0 +| hours(1)), unschedulable(1, 3, ObservationsPerNightMinimum))

  test("per night: a group with enough completions stays ongoing while its night may take an extra"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3)), min = Some(2), minN = Some(3), maxN = Some(3))
    assertEquals(
      eval(g, now = t0 +| hours(2)),
      ongoing(2, 2, nextWindow = Some(TimestampInterval.between(t0 +| hours(1), night0End)))
    )

  test("per night: a short night in progress is settled when no extras remain"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1))), minN = Some(3))
    assertEquals(eval(g, now = t0 +| hours(2)), unschedulable(2, 2, ObservationsPerNightMinimum))

  test("per night: more than the maximum on one night"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3)), maxN = Some(1))
    assertEquals(eval(g, now = t0 +| hours(2)), unschedulable(2, 3, ObservationsPerNightMaximum))

  test("per night: a night at the maximum starts the window with the next night"):
    val g = branch(1, List(done(1, t0), leaf(2)), maxN = Some(1))
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      ongoing(1, 2, nextWindow = Some(TimestampInterval.between(night0End, Timestamp.Max)))
    )

  test("per night: whether the nights to come can satisfy the limits isn't predicted"):
    // Neither 5 observations at exactly 2 per night, nor 1 at 2 per night, can
    // ever complete, but nothing is flagged until a night comes up short.
    assertEquals(eval(branch(1, List.tabulate(5)(i => leaf(i + 1)), minN = Some(2), maxN = Some(2))), pending(5))
    assertEquals(eval(branch(1, List(leaf(1)), minN = Some(2))), pending(1))

  test("per night: an OR group's extras pad out its last night"):
    // 3 of 5 with 2 per night: 2 on the first night, then 2 more, one extra.
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3), leaf(4), leaf(5)), min = Some(3), minN = Some(2))
    assertEquals(eval(g, now = t1).violationList, Nil)

  test("per night: an OR group completing on a short night must take an extra"):
    val night1End = ts("2026-10-18T00:00:00Z")
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t1), leaf(4), leaf(5)), min = Some(3), minN = Some(2))
    assertEquals(
      eval(g, now = t1 +| hours(1)),
      ongoing(3, 3, nextWindow = Some(TimestampInterval.between(t1, night1End)))
    )

  test("per night: an extra on the night an OR group completes counts toward the minimum"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t1), done(4, t1 +| hours(1)), leaf(5)), min = Some(3), minN = Some(2))
    assertEquals(eval(g, now = t1 +| hours(2)), complete(3, 3))

  test("per night: an extra on the night an OR group completes counts toward the maximum"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t0 +| hours(2))), min = Some(2), maxN = Some(2))
    assertEquals(eval(g, now = t0 +| hours(3)), unschedulable(2, 2, ObservationsPerNightMaximum))

  test("per night: extras after the night an OR group completes are ignored"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t1)), min = Some(2), minN = Some(2))
    assertEquals(eval(g, now = t1 +| hours(1)), complete(2, 2))

  test("per night: an OR group without extras to pad out its last night ends short"):
    // 3 of 4 with 2 per night, one inactive: after 2, one is required and only
    // one remains, so its night can't reach the minimum.
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3), leaf(4, inactive = true)), min = Some(3), minN = Some(2))
    assertEquals(eval(g, now = t1), ongoing(2, 3, nextWindow = Some(TimestampInterval.between(t0 +| hours(1), Timestamp.Max))))
    val h = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t1), leaf(4, inactive = true)), min = Some(3), minN = Some(2))
    assertEquals(eval(h, now = t1 +| hours(1)), unschedulable(3, 3, ObservationsPerNightMinimum))

  test("per night: a legal night that leaves a remainder no night can take"):
    // 9 divides as 4 + 5, but after 6 on the first night 3 remain, and their
    // night is short as soon as the last of them completes.
    val first = List.tabulate(6)(i => done(i + 1, t0 +| minutes(i * 10)))
    val rest  = List.tabulate(3)(i => done(i + 7, t1 +| minutes(i * 10)))
    val g     = branch(1, first ++ rest, minN = Some(4), maxN = Some(6))
    assertEquals(eval(g, now = t1 +| hours(1)), unschedulable(9, 9, ObservationsPerNightMinimum))

  test("per night: observations at more than one site"):
    val g = branch(1, List(leaf(1, site = Site.GN), leaf(2, site = Site.GS)), minN = Some(2))
    assertEquals(eval(g), unschedulable(0, 2, ObservationsPerNightMultipleSites))

  test("per night: a calibration subgroup counts as one, on the night it completes"):
    val sub = branch(2, List(done(2, t0), done(3, t0 +| hours(1))))
    val g   = branch(1, List(done(1, t0 +| hours(2)), sub), minN = Some(2), maxN = Some(2))
    assertEquals(eval(g, now = t0 +| hours(3)), complete(2, 2))

  // Subgroups

  test("a needed subgroup that is unschedulable sinks its parent"):
    val sub    = branch(2, List(leaf(2), leaf(3, inactive = true)), min = Some(2))
    val parent = branch(1, List(leaf(1), sub))
    assertEquals(
      eval(parent).violationList,
      List(SubgroupUnschedulable)
    )

  test("an unschedulable subgroup the parent doesn't need is harmless"):
    val sub    = branch(2, List(leaf(2), leaf(3, inactive = true)), min = Some(2))
    val parent = branch(1, List(done(1, t0), sub, leaf(4)), min = Some(2))
    assertEquals(eval(parent), ongoing(1, 2))

  test("a complete subgroup counts as one element, completed at its deciding completion"):
    val sub    = branch(2, List(done(2, t0 +| hours(1)), done(3, t0 +| hours(3)), leaf(4)), min = Some(2))
    // The subgroup completed at t0 + 3h, more than an hour after observation 1.
    val parent = branch(1, List(done(1, t0), sub), maxI = Some(hours(1)))
    val all    = evalAll(parent, t0 +| hours(4))
    assertEquals(all(gid(2)).status, GroupStatus.Complete)
    assertEquals(all(gid(1)).violationList, List(MaximumInterval))

  test("every group in the tree is evaluated"):
    val parent = branch(1, List(branch(2, List(leaf(1))), branch(3, List(branch(4, List(leaf(2)))))))
    assertEquals(evalAll(parent, t0).keySet, Set(1L, 2L, 3L, 4L).map(gid))

  // Input

  test("an observation without facts is not started"):
    val g = branch(1, List(done(1, t0), leaf(2)))
    assertEquals(
      GroupProgressEvaluator.evaluate(List(g.branch), g.facts - oid(2), t0)(gid(1)),
      ongoing(1, 2)
    )

  test("a minimum of zero requires every element"):
    assertEquals(eval(branch(1, List(done(1, t0), leaf(2)), min = Some(0))), ongoing(1, 2))
