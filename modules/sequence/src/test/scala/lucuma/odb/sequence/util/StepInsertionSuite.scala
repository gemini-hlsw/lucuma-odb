// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.syntax.either.*
import cats.syntax.eq.*
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

  // A step and the order in which it was started, if it was.
  private type Entry = (Row[Int], Option[Int])

  private def row(a: Int, s: Int): Entry =
    (Row(atomId(a), a, 1, stepId(a, s), s, none, none, protoStep(a, s)), none)

  extension (e: Entry)
    def started(state: StepExecutionState, order: Int, visit: Visit.Id = v1): Entry =
      (e._1.copy(state = state.some, visitId = visit.some), order.some)

    def inBatch(batch: Int): Entry =
      (e._1.copy(batch = batch), e._2)

  // Looks up the steps the planner needs, as the database queries do.
  private def plan(entries: Entry*)(
    after:     Option[Step.Id]  = none,
    reference: Option[Step.Id]  = none,
    visit:     Option[Visit.Id] = v1.some
  ): Either[String, Plan[Int]] =
    val rows   = entries.toList.map(_._1)
    val latest = entries.toList.filter(_._1.isStarted).sortBy(_._2).lastOption.map(_._1)
    val next   = rows.filterNot(_.isStarted).sortBy(r => (r.atomIndex, r.stepIndex)).headOption
    val batch  = rows.map(_.batch).maxOption.getOrElse(1)
    def find(sid: Option[Step.Id]): Option[Row[Int]] = sid.flatMap(id => rows.find(_.stepId === id))
    StepInsertion.plan(reference, find(reference), after, find(after), latest, next, visit, batch)

  // Two atoms of two steps each, nothing executed.
  private val fresh: List[Entry] =
    List(row(0, 0), row(0, 1), row(1, 0), row(1, 1))

  test("empty sequence"):
    assert(plan()().isLeft)

  test("nothing executed: before the first step, based on it and in its atom"):
    assertEquals(
      plan(fresh*)(visit = none),
      Plan(protoStep(0, 0), none, Target.Existing(atomId(0), 0, none)).asRight
    )

  test("unknown placement step"):
    assert(plan(fresh*)(stepId(9, 9).some).isLeft)

  test("unknown reference step"):
    assert(plan(fresh*)(reference = stepId(9, 9).some).isLeft)

  test("after an unstarted step in the middle of an atom"):
    assertEquals(
      plan(fresh*)(stepId(0, 0).some),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.Existing(atomId(0), 1, none)).asRight
    )

  test("after the last unstarted step of an atom: extends that atom, based on the first step by default"):
    assertEquals(
      plan(fresh*)(stepId(0, 1).some),
      Plan(protoStep(0, 0), protoStep(0, 1).some, Target.Existing(atomId(0), 2, none)).asRight
    )

  test("reference: a future step, placed after another"):
    assertEquals(
      plan(fresh*)(stepId(0, 0).some, stepId(1, 1).some),
      Plan(protoStep(1, 1), protoStep(0, 0).some, Target.Existing(atomId(0), 1, none)).asRight
    )

  test("reference: an executed step other than the most recent one, placed as the next step"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0))(reference = stepId(0, 0).some),
      Plan(protoStep(0, 0), protoStep(0, 1).some, Target.Existing(atomId(0), 2, none)).asRight
    )

  test("a not started execution state counts as unstarted"):
    assertEquals(
      plan((row(0, 0)._1.copy(state = NotStarted.some, visitId = v1.some), 1.some), row(0, 1))(),
      Plan(protoStep(0, 0), none, Target.Existing(atomId(0), 0, none)).asRight
    )

  test("mid-atom, same visit: after the executed step, before the rest of its atom"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1), row(1, 0))(),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.Existing(atomId(0), 1, none)).asRight
    )

  test("completed atom, same visit: extends the completed atom, which already precedes the pending ones"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0), row(1, 1))(),
      Plan(protoStep(0, 1), protoStep(0, 1).some, Target.Existing(atomId(0), 2, none)).asRight
    )

  test("after the most recently executed step, given explicitly: as if not given"):
    val entries = List(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0))
    assertEquals(plan(entries*)(stepId(0, 1).some), plan(entries*)())
    assert(plan(entries*)().isRight)

  test("completed atom, earlier visit: a new atom ahead of the pending ones"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0), row(1, 1))(visit = v2.some),
      Plan(protoStep(0, 1), protoStep(0, 1).some, Target.NewAtom(1)).asRight
    )

  test("partially executed atom, earlier visit: a new atom ahead of the rest of it"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(0, 1))(visit = v2.some),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.NewAtom(0)).asRight
    )

  test("completed atom of an earlier batch, same visit: a new atom ahead of the pending ones"):
    // e.g., the acquisition was reset after the atom was executed
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).inBatch(2), row(2, 0).inBatch(2))(),
      Plan(protoStep(0, 0), protoStep(0, 0).some, Target.NewAtom(1)).asRight
    )

  test("abandoned step of the current batch, same visit: extends its atom"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).started(Abandoned, 2).inBatch(2))().map(_.target),
      Target.Existing(atomId(1), 1, none).asRight
    )

  test("stale atom indices: an executed atom is moved ahead of the pending atoms"):
    // e.g., the pending atoms were renumbered from 0 by a replaceSequence
    assertEquals(
      plan(row(5, 0).started(Completed, 1), row(0, 0), row(1, 0))().map(_.target),
      Target.Existing(atomId(5), 1, 0.some).asRight
    )

  test("stale atom indices: an executed atom sharing the first pending atom's index is moved"):
    val (executed, order) = row(5, 0).started(Completed, 1)
    assertEquals(
      plan((executed.copy(atomIndex = 0), order), row(0, 0), row(1, 0))().map(_.target),
      Target.Existing(atomId(5), 1, 0.some).asRight
    )

  test("based on the most recently started step"):
    assertEquals(
      plan(row(0, 0).started(Completed, 2), row(0, 1).started(Completed, 1), row(1, 0))().map(_.reference),
      protoStep(0, 0).asRight
    )

  test("an ongoing step: after it"):
    assertEquals(
      plan(row(0, 0).started(Ongoing, 1), row(0, 1), row(1, 0))().map(p => (p.reference, p.target)),
      (protoStep(0, 0), Target.Existing(atomId(0), 1, none)).asRight
    )

  test("after the ongoing step, given explicitly: as if not given"):
    val entries = List(row(0, 0).started(Ongoing, 1), row(0, 1))
    assertEquals(plan(entries*)(stepId(0, 0).some), plan(entries*)())
    assert(plan(entries*)().isRight)

  test("not after an earlier executed step"):
    assert(plan(row(0, 0).started(Completed, 1), row(0, 1).started(Completed, 2), row(1, 0))(stepId(0, 0).some).isLeft)

  test("not after an executed step that precedes the ongoing one"):
    assert(plan(row(0, 0).started(Completed, 1), row(0, 1).started(Ongoing, 2))(stepId(0, 0).some).isLeft)

  test("everything executed, same visit: extends the last atom"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).started(Completed, 2))().map(p => (p.reference, p.target)),
      (protoStep(1, 0), Target.Existing(atomId(1), 1, none)).asRight
    )

  test("everything executed, earlier visit: a new atom at the end"):
    assertEquals(
      plan(row(0, 0).started(Completed, 1), row(1, 0).started(Completed, 2))(visit = v2.some).map(_.target),
      Target.NewAtom(2).asRight
    )
