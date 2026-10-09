// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.data.StepExecutionState
import lucuma.odb.sequence.data.ProtoStep

/**
 * Works out where new steps go when they are inserted by hand into a stored
 * (materialized) sequence, and which existing step's instrument configuration
 * they are based upon.
 */
object StepInsertion:

  /**
   * A step of a stored sequence, executed or not.
   *
   * @param atomIndex      orders atoms with unstarted steps
   * @param stepIndex      orders unstarted steps within an atom
   * @param state          execution state, if any execution event was received
   * @param executionOrder order in which started steps were executed
   * @param visitId        visit in which the step was executed, if started
   */
  final case class Row[D](
    atomId:         Atom.Id,
    atomIndex:      Int,
    stepId:         Step.Id,
    stepIndex:      Int,
    state:          Option[StepExecutionState],
    executionOrder: Option[Int],
    visitId:        Option[Visit.Id],
    step:           ProtoStep[D]
  ):

    /** Whether the step has started executing (whatever became of it). */
    def isStarted: Boolean =
      state.exists(_ =!= StepExecutionState.NotStarted)

  /** Where the new steps are written. */
  enum Target:

    /**
     * Into an existing atom starting at `stepIndex`, after shifting the
     * unstarted steps at or after that index to make room.  When `atomIndex`
     * is defined, the atom is moved to that index.
     */
    case Existing(atomId: Atom.Id, stepIndex: Int, atomIndex: Option[Int])

    /** Into a new atom of their own at `atomIndex`. */
    case NewAtom(atomIndex: Int)

  /**
   * @param reference  step whose instrument configuration the new steps are
   *                   based upon
   * @param previous   step that will precede the new steps, if any
   * @param target     where the new steps are written
   * @param shiftAtoms atoms whose index must be incremented to make room for the
   *                   target atom's (new) index
   */
  final case class Plan[D](
    reference:  ProtoStep[D],
    previous:   Option[ProtoStep[D]],
    target:     Target,
    shiftAtoms: List[Atom.Id]
  )

  /**
   * Plans the insertion of new steps immediately after `afterStepId`, which
   * must be either unstarted or the most recently started step.  When not
   * specified, the most recently started step is assumed or, if nothing has
   * started, the new steps go before the first step.
   *
   * The new steps are based on, and join the atom of, the step they follow, or
   * the first step if they follow nothing.  A started step's atom is only
   * extended if the step was executed in the current visit, otherwise the new
   * steps get an atom of their own.  Either way that atom is moved ahead of the
   * other atoms with unstarted steps, since atom indices only order unstarted
   * atoms and may be stale for executed ones.
   */
  def plan[D](
    rows:         List[Row[D]],
    afterStepId:  Option[Step.Id],
    currentVisit: Option[Visit.Id]
  ): Either[String, Plan[D]] =

    val started   = rows.filter(_.isStarted).sortBy(_.executionOrder)
    val unstarted = rows.filterNot(_.isStarted).sortBy(r => (r.atomIndex, r.stepIndex))
    val latest    = started.lastOption

    // The step after which the new steps go, if any.
    val anchor: Either[String, Option[Row[D]]] =
      afterStepId.fold(latest.asRight): sid =>
        rows.find(_.stepId === sid) match
          case None                                          =>
            s"Step $sid is not part of the sequence.".asLeft
          case Some(r) if r.isStarted && !latest.contains(r) =>
            s"Step $sid is not the most recently executed step, so nothing may be inserted after it.".asLeft
          case Some(r)                                       =>
            r.some.asRight

    // Index at which to place an atom ahead of the other atoms with unstarted
    // steps, and those other atoms, which must be shifted to make room.
    def ahead(exclude: Option[Atom.Id]): (Option[Int], List[Atom.Id]) =
      val others = unstarted.filterNot(r => exclude.contains(r.atomId))
      (others.map(_.atomIndex).minOption, others.map(_.atomId).distinct)

    anchor.flatMap:
      case None                                               =>
        unstarted
          .headOption
          .toRight("The sequence is empty, so there is no instrument configuration on which to base the new steps.")
          .map(n => Plan(n.step, none, Target.Existing(n.atomId, n.stepIndex, none), Nil))

      case Some(x) if !x.isStarted                            =>
        Plan(x.step, x.step.some, Target.Existing(x.atomId, x.stepIndex + 1, none), Nil).asRight

      case Some(x) if x.visitId.exists(currentVisit.contains) =>
        // Before any of the atom's remaining steps.
        val atomRows     = rows.filter(_.atomId === x.atomId)
        val stepIndex    = atomRows.filterNot(_.isStarted).map(_.stepIndex).minOption.getOrElse(atomRows.map(_.stepIndex).max + 1)
        val (idx, shift) = ahead(x.atomId.some)
        Plan(x.step, x.step.some, Target.Existing(x.atomId, stepIndex, idx), shift).asRight

      case Some(x)                                            =>
        val (idx, shift) = ahead(none)
        Plan(x.step, x.step.some, Target.NewAtom(idx.getOrElse(rows.map(_.atomIndex).max + 1)), shift).asRight
