// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import fs2.Pure
import fs2.Stream
import lucuma.core.model.ExecutionEvent.SequenceEvent
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.AtomRecord
import lucuma.odb.sequence.data.StepRecord
import lucuma.odb.sequence.data.VisitRecord

/**
 * A sequence generator.
 *
 * @tparam D dynamic instrument config type
 */
trait SequenceGenerator[D]:

  /**
   * Atoms and their steps for this sequence.
   */
  def generate(when: Timestamp): Stream[Pure, Atom[D]]

  def recordAtom(atom: AtomRecord): SequenceGenerator[D]

  /**
   * Records a step and returns an updated generator.
   */
  def recordStep(step: StepRecord[D])(using Eq[D]): SequenceGenerator[D]

  def recordVisit(visit: VisitRecord): SequenceGenerator[D]

  def recordSequenceEvent(cmd: SequenceEvent): SequenceGenerator[D]


object SequenceGenerator:

  abstract class Base[D] extends SequenceGenerator[D]:
    override def recordAtom(atom: AtomRecord): SequenceGenerator[D] =
      this

    override def recordStep(step:  StepRecord[D])(using Eq[D]): SequenceGenerator[D] =
      this

    override def recordVisit(visit: VisitRecord): SequenceGenerator[D] =
      this

    override def recordSequenceEvent(cmd: SequenceEvent): SequenceGenerator[D] =
      this

  /** A degenerate implementation that produces no atoms. */
  def empty[D]: SequenceGenerator[D] =
    new Base[D]:
      override def generate(when:  Timestamp): Stream[Pure, Atom[D]] =
        Stream.empty