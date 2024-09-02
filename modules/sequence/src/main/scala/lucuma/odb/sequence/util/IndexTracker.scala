// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.util

import cats.syntax.eq.*
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.sequence.data.StepRecord

sealed trait IndexTracker:
  def atomCount: Int
  def stepCount: Int

  def record[D](step: StepRecord[D]): IndexTracker

  def toTuple: (Int, Int) =
    (atomCount, stepCount)

object IndexTracker:
  case object Zero extends IndexTracker:
    override def atomCount: Int = 0
    override def stepCount: Int = 0

    override def record[D](step: StepRecord[D]): IndexTracker =
      Recording(1, step.atomId, 1, step.id)

  case class Recording(
    atomCount: Int,
    atomId:    Atom.Id,
    stepCount: Int,
    stepId:    Step.Id
  ) extends IndexTracker:

    override def record[D](step: StepRecord[D]): IndexTracker =
      if stepId === step.id then this
      else if atomId === step.atomId then copy(stepCount = stepCount + 1, stepId = step.id)
      else Recording(atomCount + 1, step.atomId, stepCount = 1, step.id)