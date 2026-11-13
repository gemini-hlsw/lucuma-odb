// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.NonEmptySet
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosShort
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupConstraintViolation.*
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.ObservationQaState
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.group.GroupProgress
import lucuma.core.model.group.GroupStanding
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
    state:    ExecutionState          = ExecutionState.NotStarted,
    time:     Option[Timestamp]       = None,
    site:     Site                    = Site.GN,
    inactive: Boolean                 = false,
    start:    Option[Timestamp]       = None,
    role:     Option[CalibrationRole] = None,
    qa:       Option[ObservationQaState] = None,
    science:  Option[Long]            = None
  ): Node =
    obs(n, ObservationFacts(state, role, science.map(oid), inactive, qa, start, time, Some(site)))

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

  test("a suspended observation is left out, but its group has started"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Ongoing, start = Some(t0), inactive = true), leaf(2)))),
      ongoing(0, 1)
    )

  test("with no minimum, suspending an executing observation lets the group complete without it"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing, start = Some(t0), inactive = true)))),
      complete(1, 1)
    )

  test("a daytime calibration that has started doesn't start its group"):
    assertEquals(
      eval(branch(1, List(leaf(1), leaf(2, ExecutionState.Ongoing, start = Some(t0), role = Some(CalibrationRole.DaytimeFlat))))),
      pending(1)
    )

  test("a subgroup whose observations were all suspended is left out, but has started its parent"):
    val sub = branch(2, List(leaf(2, ExecutionState.Ongoing, start = Some(t0), inactive = true)))
    val all = evalAll(branch(1, List(leaf(1), sub)), t0)
    assertEquals(all(gid(2)), GroupProgress.Empty)
    assertEquals(all(gid(1)), ongoing(0, 1))

  test("a suspended science observation with a telluric not yet taken has started the group holding its calibration group"):
    val science  = leaf(2, ExecutionState.Ongoing, start = Some(t0), inactive = true)
    val telluric = leaf(3, role = Some(CalibrationRole.Telluric), science = Some(2), inactive = true)
    val all      = evalAll(branch(1, List(leaf(1), branch(2, List(science, telluric)))), t0)
    assertEquals(all(gid(2)), GroupProgress.Empty)
    assertEquals(all(gid(1)), ongoing(0, 1))

  // QA

  // A completed observation, started and completed at `t`, with QA state `qa`.
  def assessed(n: Long, t: Timestamp, qa: ObservationQaState, role: Option[CalibrationRole] = None, science: Option[Long] = None): Node =
    leaf(n, ExecutionState.Completed, Some(t), start = Some(t), role = role, qa = Some(qa), science = science)

  test("a usable completion doesn't count, but the group has started"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Usable), leaf(2)), min = Some(1))),
      ongoing(0, 1)
    )

  test("an ordered group has started with an element past its first required executing or rejected"):
    val g = (second: Node) => branch(1, List(leaf(1), second), min = Some(1), ordered = true)
    assertEquals(eval(g(leaf(2, ExecutionState.Ongoing, start = Some(t0))), t0 +| hours(1)), ongoing(0, 1))
    assertEquals(eval(g(assessed(2, t0, ObservationQaState.Fail)), t0 +| hours(1)), ongoing(0, 1))
    assertEquals(eval(g(leaf(2)), t0 +| hours(1)), pending(1))

  test("a failed completion doesn't count"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Fail), leaf(2, ExecutionState.Ongoing)), min = Some(1))),
      ongoing(0, 1)
    )

  test("passing and unassessed completions count"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Pass), done(2, t0 +| hours(1))))),
      complete(2, 2)
    )

  test("a rejected completion can leave too few viable elements"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Usable), leaf(2)), min = Some(2))),
      unschedulable(0, 2, InsufficientElements)
    )

  test("a rejected completion isn't checked against the intervals"):
    val g = branch(
      1,
      List(done(1, t0), assessed(2, t0 +| minutes(30), ObservationQaState.Fail), done(3, t0 +| hours(2))),
      min  = Some(2),
      minI = Some(hours(1))
    )
    assertEquals(eval(g, now = t0 +| hours(3)), complete(2, 2))

  test("an observation still executing counts as started whatever its QA so far"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Ongoing, qa = Some(ObservationQaState.Fail))))),
      ongoing(0, 1)
    )

  test("with no minimum, a rejected completion is still required"):
    assertEquals(
      eval(branch(1, List(done(1, t0), assessed(2, t0 +| hours(1), ObservationQaState.Usable))), t0 +| hours(2)),
      unschedulable(1, 2, InsufficientElements)
    )

  test("with no minimum, a group with only rejected completions is unschedulable, not empty"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Usable), assessed(2, t0, ObservationQaState.Fail))), t0 +| hours(1)),
      unschedulable(0, 2, InsufficientElements)
    )

  test("with no minimum, a replacement doesn't make up for a rejected completion"):
    assertEquals(
      eval(branch(1, List(done(1, t0), assessed(2, t0, ObservationQaState.Fail), done(3, t0 +| hours(1)))), t0 +| hours(2)),
      unschedulable(2, 3, InsufficientElements)
    )

  test("ordered with no minimum, a rejected completion is still required"):
    assertEquals(
      eval(branch(1, List(assessed(1, t0, ObservationQaState.Fail), done(2, t0 +| hours(1))), ordered = true), t0 +| hours(2)),
      unschedulable(1, 2, InsufficientElements)
    )

  test("with a minimum, a replacement takes a rejected completion's place"):
    val g = (third: Node) => branch(1, List(done(1, t0), assessed(2, t0, ObservationQaState.Fail), third), min = Some(2))
    assertEquals(eval(g(leaf(3)), t0 +| hours(1)), ongoing(1, 2))
    assertEquals(eval(g(done(3, t0 +| hours(1))), t0 +| hours(2)), complete(2, 2))

  test("ordered with a minimum, a replacement takes a rejected completion's place"):
    val g = (third: Node) => branch(1, List(assessed(1, t0, ObservationQaState.Fail), done(2, t0 +| hours(1)), third), min = Some(2), ordered = true)
    assertEquals(eval(g(leaf(3)), t0 +| hours(2)), ongoing(1, 2))
    assertEquals(eval(g(done(3, t0 +| hours(2))), t0 +| hours(3)), complete(2, 2))

  test("a calibration group goes with its rejected science observation"):
    val telluric = leaf(3, ExecutionState.Completed, Some(t0 +| hours(1)), start = Some(t0 +| hours(1)), role = Some(CalibrationRole.Telluric), science = Some(2))
    val sub      = branch(2, List(assessed(2, t0, ObservationQaState.Usable), telluric))
    val all      = evalAll(branch(1, List(done(1, t0), sub)), t0 +| hours(2))
    assertEquals(all(gid(2)), unschedulable(0, 2, InsufficientElements))
    assertEquals(all(gid(1)), unschedulable(1, 2, InsufficientElements))

  test("a rejected calibration group is left out of a parent with a minimum"):
    val telluric = leaf(3, ExecutionState.Completed, Some(t0 +| hours(1)), start = Some(t0 +| hours(1)), role = Some(CalibrationRole.Telluric), science = Some(2))
    val sub      = branch(2, List(assessed(2, t0, ObservationQaState.Usable), telluric))
    val all      = evalAll(branch(1, List(sub, done(1, t0)), min = Some(1), ordered = true), t0 +| hours(2))
    assertEquals(all(gid(1)), complete(1, 1))

  test("a calibration group's telluric still to do goes with its rejected science observation"):
    val telluric = leaf(3, role = Some(CalibrationRole.Telluric), science = Some(2))
    val sub      = branch(2, List(assessed(2, t0, ObservationQaState.Fail), telluric))
    val all      = evalAll(branch(1, List(sub, leaf(4)), min = Some(1)), t0 +| hours(1))
    assertEquals(all(gid(2)), unschedulable(0, 2, InsufficientElements))
    assertEquals(all(gid(1)), ongoing(0, 1))

  test("a calibration group goes with its science observation declared complete without being observed"):
    val telluric = leaf(3, role = Some(CalibrationRole.Telluric), science = Some(2))
    val sub      = (qa: Option[ObservationQaState]) => branch(2, List(leaf(2, ExecutionState.DeclaredComplete, qa = qa), telluric))
    List(None, Some(ObservationQaState.Fail)).foreach: qa =>
      val all = evalAll(branch(1, List(done(1, t0), sub(qa))), t0 +| hours(1))
      assertEquals(all(gid(2)), GroupProgress.Empty, clue(qa))
      assertEquals(all(gid(1)), complete(1, 1), clue(qa))

  test("a completed telluric goes with its inactive science observation"):
    val telluric = leaf(3, ExecutionState.Completed, Some(t0), start = Some(t0), role = Some(CalibrationRole.Telluric), science = Some(2))
    val all      = evalAll(branch(1, List(done(1, t0), branch(2, List(leaf(2, inactive = true), telluric)))), t0 +| hours(1))
    assertEquals(all(gid(2)), GroupProgress.Empty)
    assertEquals(all(gid(1)), complete(1, 1))

  test("a telluric's own QA doesn't affect its calibration group"):
    val telluric = assessed(3, t0 +| hours(1), ObservationQaState.Fail, role = Some(CalibrationRole.Telluric), science = Some(2))
    val sub      = branch(2, List(done(2, t0), telluric))
    assertEquals(eval(sub, t0 +| hours(2)), complete(2, 2))

  test("a calibration goes with its associated science observation, not the first in its group"):
    // Not something the API allows, but the association, not the group's
    // order, decides.
    val telluric = leaf(4, ExecutionState.Completed, Some(t0 +| hours(1)), start = Some(t0 +| hours(1)), role = Some(CalibrationRole.Telluric), science = Some(3))
    val sub      = branch(2, List(done(2, t0), assessed(3, t0, ObservationQaState.Fail), telluric))
    assertEquals(eval(branch(1, List(sub), min = Some(1)), t0 +| hours(2)), unschedulable(0, 1, SubgroupUnschedulable))
    assertEquals(evalAll(sub, t0 +| hours(2))(gid(2)), unschedulable(1, 3, InsufficientElements))

  test("an observation declared complete is rejected for its QA like a completed one"):
    val declared = leaf(1, ExecutionState.DeclaredComplete, Some(t0), start = Some(t0), qa = Some(ObservationQaState.Usable))
    assertEquals(
      eval(branch(1, List(declared, done(2, t0 +| hours(1)))), t0 +| hours(2)),
      unschedulable(1, 2, InsufficientElements)
    )

  test("a subgroup with only rejected observations is rejected in its parent"):
    val sub = branch(2, List(assessed(2, t0, ObservationQaState.Fail), assessed(3, t0, ObservationQaState.Usable)))
    val all = evalAll(branch(1, List(done(1, t0), sub)), t0 +| hours(1))
    assertEquals(all(gid(2)), unschedulable(0, 2, InsufficientElements))
    assertEquals(all(gid(1)), unschedulable(1, 2, InsufficientElements))
    // Left out of an ordered parent with a minimum, rather than blocking it.
    assertEquals(eval(branch(1, List(sub, done(1, t0)), min = Some(1), ordered = true), t0 +| hours(1)), complete(1, 1))

  test("a subgroup with some rejected observations is unschedulable, not rejected, in its parent"):
    val sub = branch(2, List(done(2, t0), assessed(3, t0, ObservationQaState.Fail)))
    val all = evalAll(branch(1, List(sub, done(1, t0)), min = Some(1), ordered = true), t0 +| hours(1))
    assertEquals(all(gid(2)), unschedulable(1, 2, InsufficientElements))
    // The observation after it, which never counts, completed first as well.
    assertEquals(all(gid(1)), unschedulable(0, 1, Order, SubgroupUnschedulable))

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
    // The third element never counts, but completing it before the second is
    // out of order.
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2), done(3, t0 +| hours(1))), min = Some(2), ordered = true)),
      unschedulable(1, 2, Order)
    )

  test("ordered n of m: an element after the first n completing first is out of order"):
    assertEquals(
      eval(branch(1, List(done(1, t0 +| hours(1)), done(2, t0 +| hours(2)), done(3, t0)), min = Some(2), ordered = true)),
      unschedulable(2, 2, Order)
    )

  test("ordered n of m: elements after the first n completing after them, in order, are extra"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t0 +| hours(2))), min = Some(2), ordered = true)),
      complete(2, 2)
    )
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t0 +| hours(2)), done(4, t0 +| hours(3))), min = Some(2), ordered = true)),
      complete(2, 2)
    )

  test("ordered n of m: elements after the first n are held to the order among themselves"):
    // The fourth completes while the third is still to do, or before it.
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3), done(4, t0 +| hours(2))), min = Some(2), ordered = true)),
      unschedulable(2, 2, Order)
    )
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t0 +| hours(3)), done(4, t0 +| hours(2))), min = Some(2), ordered = true)),
      unschedulable(2, 2, Order)
    )

  test("ordered n of m with per-night limits: an element after the first n completing first is out of order at once"):
    // The surplus element completes before the first starts, closer than the
    // minimum interval: the group is unschedulable as soon as it completes,
    // not only once the count is met.
    val g = (second: Node) =>
      branch(1, List(done(1, t0 +| hours(2), start = Some(t0 +| hours(1))), second, done(3, t0)), min = Some(2), ordered = true, minI = Some(hours(3)), minN = Some(2))
    assertEquals(eval(g(leaf(2)), now = t0 +| hours(3)).violationList, List(Order))
    assert(eval(g(done(2, t0 +| hours(5))), now = t0 +| hours(6)).violationList.contains(Order))

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

  test("suspending an element that started too soon takes back the violation"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing, start = Some(t0 +| minutes(30)), inactive = true), leaf(3)), minI = Some(hours(1)))
    assertEquals(
      eval(g, now = t0 +| hours(1)),
      ongoing(1, 2, nextWindow = Some(TimestampInterval.from(t0 +| hours(1))))
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

  // 2 of 4 required, 3 per night, completions an hour apart.
  def shortNight(extra: Node, minI: Option[TimeSpan] = None, maxI: Option[TimeSpan] = None): Node =
    branch(1, List(done(1, t0), done(2, t0 +| hours(1)), extra, leaf(4)), min = Some(2), minI = minI, maxI = maxI, minN = Some(3))

  test("per night: a short night's extra must complete within the maximum interval"):
    val g = shortNight(leaf(3), maxI = Some(hours(2)))
    assertEquals(
      eval(g, now = t0 +| hours(2)),
      ongoing(2, 2, nextWindow = Some(TimestampInterval.between(t0 +| hours(1), t0 +| hours(3))))
    )
    assertEquals(eval(g, now = t0 +| hours(4)), unschedulable(2, 2, MaximumInterval))

  test("per night: a short night's extra completed within the maximum interval completes the group"):
    assertEquals(eval(shortNight(done(3, t0 +| hours(2)), maxI = Some(hours(2))), now = t0 +| hours(5)), complete(2, 2))

  test("per night: a short night's extra completed after the maximum interval doesn't complete the group"):
    assertEquals(
      eval(shortNight(done(3, t0 +| hours(4)), maxI = Some(hours(2))), now = t0 +| hours(5)),
      unschedulable(2, 2, MaximumInterval)
    )

  test("per night: a short night's extra started before the minimum interval"):
    // The two completions are the minimum interval apart, and the extra starts
    // an hour after the second.
    val g = (start: Timestamp) =>
      branch(1, List(done(1, t0), done(2, t0 +| hours(2)), leaf(3, ExecutionState.Ongoing, start = Some(start)), leaf(4)), min = Some(2), minI = Some(hours(2)), minN = Some(3))
    assertEquals(eval(g(t0 +| hours(3)), now = t0 +| hours(3)), unschedulable(2, 2, MinimumInterval))
    assertEquals(eval(g(t0 +| hours(4)), now = t0 +| hours(5)).violationList, Nil)

  test("per night: an extra on the night its group completes is held to the maximum interval"):
    // The night already has its 2, but the third completion counts toward it.
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1)), done(3, t0 +| hours(3))), min = Some(2), maxI = Some(hours(1)), minN = Some(2))
    assertEquals(eval(g, now = t0 +| hours(4)), unschedulable(2, 2, MaximumInterval))

  test("per night: a short night in progress is settled when no extras remain"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(1))), minN = Some(3))
    assertEquals(eval(g, now = t0 +| hours(2)), unschedulable(2, 2, ObservationsPerNightMinimum))

  test("per night: a short night in progress is settled when too few remain, even short of the count"):
    // One done tonight and only one left, so tonight can reach 2 of 3 at best.
    val g = branch(1, List(done(1, t0), leaf(2)), minN = Some(3))
    assertEquals(eval(g, now = t0 +| hours(1)), unschedulable(1, 2, ObservationsPerNightMinimum))

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

  // QA and time constraints: the completions left after a rejection are
  // checked without it, and a replacement observed later can't undo that.

  test("a rejection can leave a night that has ended short of the minimum"):
    val g = (qa: ObservationQaState) =>
      branch(1, List(done(1, t0), assessed(2, t0 +| hours(1), qa), done(3, t1), done(4, t1 +| hours(1)), leaf(5)), min = Some(4), minN = Some(2))
    assertEquals(eval(g(ObservationQaState.Pass), now = t1 +| hours(2)), complete(4, 4))
    assertEquals(eval(g(ObservationQaState.Usable), now = t1 +| hours(2)), unschedulable(3, 4, ObservationsPerNightMinimum))

  test("a rejection in a same-night group can't be replaced on another night"):
    val g = (qa: ObservationQaState) =>
      branch(1, List(done(1, t0), assessed(2, t0 +| hours(1), qa), done(3, t1)), min = Some(2), sameNight = true)
    assertEquals(eval(g(ObservationQaState.Pass), now = t1 +| hours(1)), complete(2, 2))
    assertEquals(eval(g(ObservationQaState.Fail), now = t1 +| hours(1)), unschedulable(2, 2, SameNight))

  test("a rejection can leave completions further apart than the maximum interval"):
    val g = (qa: ObservationQaState) =>
      branch(1, List(done(1, t0), assessed(2, t0 +| hours(1), qa), done(3, t0 +| hours(3))), min = Some(2), maxI = Some(hours(2)))
    assertEquals(eval(g(ObservationQaState.Pass), now = t0 +| hours(4)), complete(2, 2))
    assertEquals(eval(g(ObservationQaState.Fail), now = t0 +| hours(4)), unschedulable(2, 2, MaximumInterval))

  test("an ordered group's replacement can't take a rejected element's place once a later one is complete"):
    val rejected = assessed(1, t0, ObservationQaState.Fail)
    val later    = done(2, t0 +| hours(1))
    val inPlace  = branch(1, List(rejected, leaf(3), later), min = Some(2), ordered = true)
    val appended = (r: Node) => branch(1, List(rejected, later, r), min = Some(2), ordered = true)
    assertEquals(eval(inPlace, now = t0 +| hours(2)), unschedulable(1, 2, Order))
    assertEquals(eval(appended(leaf(3)), now = t0 +| hours(2)), ongoing(1, 2))
    assertEquals(eval(appended(done(3, t0 +| hours(2))), now = t0 +| hours(3)), complete(2, 2))

  // A GNIRS calibration group: science observation `n` completed at `t`, a
  // telluric `n + 1` completed at `telluric`, if any, and a daytime pinhole
  // flat `n + 2` completed at `pinhole`, if it has been.
  def gnirsCalibrationGroup(g: Long, n: Long, t: Timestamp, telluric: Option[Timestamp] = None, pinhole: Option[Timestamp] = None): Node =
    branch(
      g,
      List(done(n, t)) ++
      telluric.toList.map(tt => leaf(n + 1, ExecutionState.Completed, Some(tt), start = Some(tt), role = Some(CalibrationRole.Telluric), science = Some(n))) :+
      leaf(
        n + 2,
        pinhole.fold(ExecutionState.NotStarted)(_ => ExecutionState.Completed),
        pinhole,
        start   = pinhole,
        role    = Some(CalibrationRole.DaytimePinhole),
        science = Some(n)
      )
    )

  // A Flamingos-2 imaging calibration group: science observation `n` completed
  // at `t` and a daytime flat `n + 1` completed at `flat`, if it has been.
  def f2ImagingCalibrationGroup(g: Long, n: Long, t: Timestamp, flat: Option[Timestamp] = None): Node =
    branch(
      g,
      List(
        done(n, t),
        leaf(
          n + 1,
          flat.fold(ExecutionState.NotStarted)(_ => ExecutionState.Completed),
          flat,
          start = flat,
          role  = Some(CalibrationRole.DaytimeFlat)
        )
      )
    )

  // A spectrophotometric standard's group: the standard `n` completed at `t`
  // and its daytime arc `n + 1` completed at `arc`, if it has been.
  def specPhotCalibrationGroup(g: Long, n: Long, t: Timestamp, arc: Option[Timestamp] = None): Node =
    branch(
      g,
      List(
        leaf(n, ExecutionState.Completed, Some(t), start = Some(t), role = Some(CalibrationRole.SpectroPhotometric)),
        leaf(
          n + 1,
          arc.fold(ExecutionState.NotStarted)(_ => ExecutionState.Completed),
          arc,
          start   = arc,
          role    = Some(CalibrationRole.DaytimeArc),
          science = Some(n)
        )
      )
    )

  // 15:00 HST on 16 October, the afternoon after t0's night: the next night.
  val nextAfternoon: Timestamp = ts("2026-10-17T01:00:00Z")

  test("a daytime calibration taken the next afternoon leaves its group on the science observation's night"):
    val sub = gnirsCalibrationGroup(2, 2, t0, pinhole = Some(nextAfternoon))
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    assertEquals(eval(g, now = nextAfternoon +| hours(1)), complete(2, 2))

  test("a daytime calibration still to do doesn't hold up its group"):
    val sub = gnirsCalibrationGroup(2, 2, t0)
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    val all = evalAll(g, now = night0End +| hours(1))
    assertEquals(all(gid(2)), complete(1, 1))
    assertEquals(all(gid(1)), complete(2, 2))

  test("a daytime flat taken the next afternoon leaves its group on the science observation's night"):
    val sub = f2ImagingCalibrationGroup(2, 2, t0, flat = Some(nextAfternoon))
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    assertEquals(eval(g, now = nextAfternoon +| hours(1)), complete(2, 2))

  test("a daytime arc taken the next afternoon leaves its group on the standard's night"):
    val sub = specPhotCalibrationGroup(2, 2, t0, arc = Some(nextAfternoon))
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    assertEquals(eval(g, now = nextAfternoon +| hours(1)), complete(2, 2))

  test("a daytime arc still to do doesn't hold up the standard's group"):
    val sub = specPhotCalibrationGroup(2, 2, t0)
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    val all = evalAll(g, now = night0End +| hours(1))
    assertEquals(all(gid(2)), complete(1, 1))
    assertEquals(all(gid(1)), complete(2, 2))

  test("a daytime flat still to do doesn't hold up its group"):
    val sub = f2ImagingCalibrationGroup(2, 2, t0)
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), minN = Some(2))
    val all = evalAll(g, now = night0End +| hours(1))
    assertEquals(all(gid(2)), complete(1, 1))
    assertEquals(all(gid(1)), complete(2, 2))

  test("same night: a daytime calibration taken the next afternoon is ignored"):
    val sub = gnirsCalibrationGroup(2, 2, t0, pinhole = Some(nextAfternoon))
    val g   = branch(1, List(done(1, t0 +| hours(1)), sub), sameNight = true)
    assertEquals(eval(g, now = nextAfternoon +| hours(1)), complete(2, 2))

  test("a telluric observed after its science observation completes the calibration group"):
    val sub = gnirsCalibrationGroup(2, 2, t0 +| minutes(30), telluric = Some(t0 +| hours(2)), pinhole = Some(nextAfternoon))
    val g   = branch(1, List(done(1, t0), sub), maxI = Some(hours(1)))
    assertEquals(eval(g, now = nextAfternoon +| hours(1)), unschedulable(2, 2, MaximumInterval))

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

  // Standing

  def standings(n: Node): Map[Group.Id, GroupStanding] =
    GroupProgressEvaluator.standings(List(n.branch), evalAll(n))

  // Complete: one of two required, and it's done.
  def complete(n: Long, o: Long): Node =
    branch(n, List(done(o, t0), leaf(o + 1)), min = Some(1))

  // Unschedulable: two required, but one of the two is inactive.
  def unschedulable(n: Long, o: Long): Node =
    branch(n, List(leaf(o), leaf(o + 1, inactive = true)), min = Some(2))

  test("standing: a group that is neither complete nor unschedulable is viable"):
    assertEquals(standings(branch(1, List(leaf(1), leaf(2)))), Map(gid(1) -> GroupStanding.Viable))

  test("standing: a complete group decides for itself and its subgroups"):
    val g = branch(1, List(done(1, t0), branch(2, List(leaf(2)))), min = Some(1))
    assertEquals(
      standings(g),
      Map(gid(1) -> GroupStanding.Complete(gid(1)), gid(2) -> GroupStanding.Complete(gid(1)))
    )

  test("standing: a complete subgroup of a viable group decides only for itself"):
    val g = branch(1, List(complete(2, 1), leaf(3)))
    assertEquals(
      standings(g),
      Map(gid(1) -> GroupStanding.Viable, gid(2) -> GroupStanding.Complete(gid(2)))
    )

  test("standing: a viable group under an unschedulable parent is unschedulable"):
    // The parent needs both subgroups, and one of them can no longer complete.
    val g = branch(1, List(branch(2, List(done(1, t0), leaf(2))), unschedulable(3, 3)))
    assertEquals(evalAll(g)(gid(2)).status, GroupStatus.Ongoing)
    assertEquals(
      standings(g),
      Map(
        gid(1) -> GroupStanding.Unschedulable(gid(1)),
        gid(2) -> GroupStanding.Unschedulable(gid(1)),
        gid(3) -> GroupStanding.Unschedulable(gid(1))
      )
    )

  test("standing: the outer group decides when a complete group holds an unschedulable one"):
    val g = branch(1, List(done(1, t0), unschedulable(2, 2)), min = Some(1))
    assertEquals(standings(g)(gid(2)), GroupStanding.Complete(gid(1)))

  test("standing: the outer group decides when an unschedulable group holds a complete one"):
    val g = branch(1, List(complete(2, 1), leaf(3), leaf(4, inactive = true)), min = Some(3))
    assertEquals(standings(g)(gid(2)), GroupStanding.Unschedulable(gid(1)))
