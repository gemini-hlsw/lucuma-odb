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
   * @param atomId    atom to which the step belongs
   * @param atomIndex orders atoms with unstarted steps
   * @param batch     run of the sequence to which the atom belongs, the
   *                  highest being the current one
   * @param stepId    the step itself
   * @param stepIndex orders unstarted steps within an atom
   * @param state     execution state, if any execution event was received
   * @param visitId   visit in which the step was executed, if started
   * @param step      instrument, step and telescope configuration
   */
  final case class Row[D](
    atomId:    Atom.Id,
    atomIndex: Int,
    batch:     Int,
    stepId:    Step.Id,
    stepIndex: Int,
    state:     Option[StepExecutionState],
    visitId:   Option[Visit.Id],
    step:      ProtoStep[D]
  ):

    /** Whether the step has started executing (whatever became of it). */
    def isStarted: Boolean =
      state.exists(_ =!= StepExecutionState.NotStarted)

  /** Where the new steps are written. */
  enum Target:

    /**
     * Into an existing atom starting at `stepIndex`, after shifting the
     * unstarted steps at or after that index to make room.  When `atomIndex`
     * is defined, the atom is moved to that index ahead of the atoms with
     * unstarted steps.
     */
    case Existing(atomId: Atom.Id, stepIndex: Int, atomIndex: Option[Int])

    /** Into a new atom of their own at `atomIndex`, ahead of the atoms with unstarted steps. */
    case NewAtom(atomIndex: Int)

  /**
   * @param reference step whose instrument configuration the new steps are
   *                  based upon
   * @param previous  step that will precede the new steps, if any
   * @param target    where the new steps are written
   */
  final case class Plan[D](
    reference: ProtoStep[D],
    previous:  Option[ProtoStep[D]],
    target:    Target
  )

  /**
   * Plans the insertion of new steps, based on the instrument configuration of
   * `referenceStepId`, immediately after `afterStepId`.
   *
   * The reference may be any step of the sequence, executed or not.  When not
   * specified, the most recently started step is assumed or, if nothing has
   * started, the first step.
   *
   * The new steps may follow a step that has not started, and then join its
   * atom.  Otherwise they become the next steps to execute, which is also what
   * happens when `afterStepId` is not specified or is the most recently started
   * step: they go after the most recently started step or, if nothing has
   * started, before the first step and in its atom.  A started step's atom is
   * only extended if it belongs to the current batch (i.e., the current run of
   * the sequence, which for example an acquisition reset ends) and the step
   * was executed in the current visit.  Otherwise the new steps get an atom of
   * their own.  Either way that atom must precede the atoms with unstarted
   * steps.  Since atom indices only order unstarted atoms and may be stale for
   * executed ones, an executed atom is moved ahead of them unless its index
   * already places it there.
   *
   * @param referenceStep the step identified by `referenceStepId`, if it exists
   * @param afterStep     the step identified by `afterStepId`, if it exists
   * @param latest        the most recently started step, if any
   * @param next          the first unstarted step, if any, which is the one
   *                      that would otherwise execute next
   * @param currentVisit  the visit in which the observation is currently being
   *                      executed, if any
   * @param currentBatch  the batch that new atoms join
   */
  def plan[D](
    referenceStepId: Option[Step.Id],
    referenceStep:   Option[Row[D]],
    afterStepId:     Option[Step.Id],
    afterStep:       Option[Row[D]],
    latest:          Option[Row[D]],
    next:            Option[Row[D]],
    currentVisit:    Option[Visit.Id],
    currentBatch:    Int
  ): Either[String, Plan[D]] =

    val empty: String =
      "The sequence is empty, so there is no instrument configuration on which to base the new steps."

    def notInSequence(sid: Step.Id): String =
      s"Step $sid is not part of the sequence."

    val reference: Either[String, ProtoStep[D]] =
      referenceStepId.fold(latest.orElse(next).toRight(empty)): sid =>
        referenceStep.toRight(notInSequence(sid))
      .map(_.step)

    // Whether the step was executed in the run of the sequence and the visit
    // that are still under way.
    def isCurrent(r: Row[D]): Boolean =
      r.batch === currentBatch && r.visitId.exists(currentVisit.contains)

    // The step that will precede the new steps, if any, and where they go when
    // they are to be the next steps to execute.
    val nextPosition: Either[String, (Option[ProtoStep[D]], Target)] =
      latest match
        case None                    =>
          next.toRight(empty).map(n => (none, Target.Existing(n.atomId, n.stepIndex, none)))

        // As the most recently started step, `a` is followed by `next`,
        // whose atom is first among those with unstarted steps.  If a's
        // atom is another one, it only has to move when its index doesn't
        // already precede that atom's.
        case Some(a) if isCurrent(a) =>
          val target = next match
            case Some(n) if n.atomId === a.atomId => Target.Existing(a.atomId, n.stepIndex, none)
            case Some(n)                          => Target.Existing(a.atomId, a.stepIndex + 1, Option.when(a.atomIndex >= n.atomIndex)(n.atomIndex))
            case None                             => Target.Existing(a.atomId, a.stepIndex + 1, none)
          (a.step.some, target).asRight

        case Some(a)                 =>
          (a.step.some, Target.NewAtom(next.fold(a.atomIndex + 1)(_.atomIndex))).asRight

    val position: Either[String, (Option[ProtoStep[D]], Target)] =
      afterStepId.fold(nextPosition): sid =>
        afterStep match
          case None                                       =>
            notInSequence(sid).asLeft
          case Some(_) if latest.exists(_.stepId === sid) =>
            nextPosition
          case Some(r) if r.isStarted                     =>
            s"Step $sid was executed before the most recently executed step, so nothing may be inserted after it.".asLeft
          case Some(r)                                    =>
            (r.step.some, Target.Existing(r.atomId, r.stepIndex + 1, none)).asRight

    for
      r      <- reference
      (p, t) <- position
    yield Plan(r, p, t)
