// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.ObserveClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Gid
import lucuma.core.util.Uid
import lucuma.odb.data.StepExecutionState
import lucuma.odb.data.StepExecutionState.*
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.StepInsertion.Plan
import lucuma.odb.sequence.util.StepInsertion.Row
import lucuma.odb.sequence.util.StepInsertion.Target
import munit.FunSuite

import java.util.UUID

class StepInsertionSuite extends FunSuite:

  private def atomId(a: Int): Atom.Id =
    Uid[Atom.Id].isoUuid.reverseGet(new UUID(0L, a.toLong))

  private def stepId(a: Int, s: Int): Step.Id =
    Uid[Step.Id].isoUuid.reverseGet(new UUID(1L, (a * 100 + s).toLong))

  private val v1: Visit.Id = Gid[Visit.Id].fromLong.getOption(1L).get
  private val v2: Visit.Id = Gid[Visit.Id].fromLong.getOption(2L).get

  // The step's instrument config identifies it as atom * 100 + step.
  private def protoStep(a: Int, s: Int): ProtoStep[Int] =
    ProtoStep(a * 100 + s, StepConfig.Science, TelescopeConfig.Default, ObserveClass.Science)

  private def row(a: Int, s: Int): Row[Int] =
    Row(atomId(a), a, stepId(a, s), s, none, none, none, protoStep(a, s))

  extension (r: Row[Int])
    def started(state: StepExecutionState, order: Int, visit: Visit.Id = v1): Row[Int] =
      r.copy(state = state.some, executionOrder = order.some, visitId = visit.some)

  private def plan(rows: Row[Int]*)(after: Option[Step.Id] = none, visit: Option[Visit.Id] = v1.some): Either[String, Plan[Int]] =
    StepInsertion.plan(rows.toList, after, visit)

  // Two atoms of two steps each, nothing executed.
  private val fresh: List[Row[Int]] =
    List(row(0, 0), row(0, 1), row(1, 0), row(1, 1))

  test("empty sequence"):
    assert(plan()().isLeft)

  test("nothing executed: before the first step, based on it and in its atom"):
    assertEquals(
      plan(fresh*)(visit = none),
      Plan(protoStep(0, 0), none, Target.Existing(atomId(0), 0, none), Nil).asRight
    )

  test("unknown step"):
    assert(plan(fresh*)(stepId(9, 9).some).isLeft)

  test("after an unstarted step in the middle of an atom"):
    assertEquals(
      plan(fresh*)(stepId(0, 0).some),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.Existing(atomId(0), 1, none), Nil).asRight
    )

  test("after the last unstarted step of an atom: extends that atom"):
    assertEquals(
      plan(fresh*)(stepId(0, 1).some),
      Plan(protoStep(0, 1), protoStep(0, 1).some, Target.Existing(atomId(0), 2, none), Nil).asRight
    )

  test("a not started execution state counts as unstarted"):
    assertEquals(
      plan(row(0, 0).copy(state = NotStarted.some, executionOrder = 1.some, visitId = v1.some), row(0, 1))(),
      Plan(protoStep(0, 0), none, Target.Existing(atomId(0), 0, none), Nil).asRight
    )

  test("mid-atom, same visit: after the executed step, in its atom, which is moved ahead"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1), row(1, 0))(),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.Existing(atomId(0), 1, 1.some), List(atomId(1))).asRight
    )

  test("completed atom, same visit: extends the completed atom"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0), row(1, 1))(),
      Plan(protoStep(0, 1), protoStep(0, 1).some, Target.Existing(atomId(0), 2, 1.some), List(atomId(1))).asRight
    )

  test("completed atom, same visit, given explicitly"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0))(stepId(0, 1).some).map(_.target),
      Target.Existing(atomId(0), 2, 1.some).asRight
    )

  test("completed atom, earlier visit: a new atom ahead of the pending ones"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0), row(1, 1))(visit = v2.some),
      Plan(protoStep(0, 1), protoStep(0, 1).some, Target.NewAtom(1), List(atomId(1))).asRight
    )

  test("partially executed atom, earlier visit: a new atom ahead of the rest of it"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1))(visit = v2.some),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.NewAtom(0), List(atomId(0))).asRight
    )

  test("stale atom indices: an executed atom is moved ahead of the pending atoms"):
    // e.g., the pending atoms were renumbered from 0 by a replaceSequence
    val executed = Row(atomId(5), 5, stepId(5, 0), 0, none, none, none, protoStep(5, 0)).started(Completed, 1)
    assertEquals(
      plan(executed, row(0, 0), row(1, 0))().map(p => (p.target, p.shiftAtoms)),
      (Target.Existing(atomId(5), 1, 0.some), List(atomId(0), atomId(1))).asRight
    )

  test("started steps are ordered by execution order"):
    assertEquals(
      plan(row(0, 0).started(Completed, 2), row(0, 1).started(Completed, 1), row(1, 0))().map(_.reference),
      protoStep(0, 0).asRight
    )

  test("an ongoing step: after it"):
    assertEquals(
      plan(row(0, 0).started(Ongoing, 1), row(0, 1), row(1, 0))().map(p => (p.reference, p.target)),
      (protoStep(0, 0), Target.Existing(atomId(0), 1, 1.some)).asRight
    )

  test("after the ongoing step, given explicitly"):
    assert(plan(row(0, 0).started(Ongoing, 1), row(0, 1))(stepId(0, 0).some).isRight)

  test("not after an earlier executed step"):
    assert(plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0))(stepId(0, 0).some).isLeft)

  test("not after an executed step that precedes the ongoing one"):
    assert(plan(row(0, 0).started(Completed, 1), row(0, 1).started(Ongoing, 2))(stepId(0, 0).some).isLeft)

  test("everything executed, same visit: extends the last atom"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).started(Completed, 2))().map(p => (p.reference, p.target, p.shiftAtoms)),
      (protoStep(1, 0), Target.Existing(atomId(1), 1, none), Nil).asRight
    )

  test("everything executed, earlier visit: a new atom at the end"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).started(Completed, 2))(visit = v2.some).map(p => (p.target, p.shiftAtoms)),
      (Target.NewAtom(2), Nil).asRight
    )
