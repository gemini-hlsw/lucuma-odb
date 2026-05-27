// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.visitor

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan

import scala.collection.immutable.SortedSet

object VisitorExecutionDigestCalculator:

  case class Overheads(setup: TimeSpan, readout: TimeSpan)

 /**
  * Computes the time estimate for a resident visitors (Alopeke, Zorro, maroon-x).
  * There is no sequence for these modes. We get the teotal time as:
  *
  * total time is `setup + count * (exposureTime + readout)` derived
  * from the single science ExposureTimeMode row.
  *
  * Acquisition and reacquisition times are zero for resident visitors.
  */
  def residentDigest(
    overheads: Overheads,
    science:   Option[ExposureTimeMode],
    state:     ExecutionState
  ): ExecutionDigest =
    val (exposureTotal, count) =
      science match
        case Some(ExposureTimeMode.TimeAndCountMode(t, c, _)) =>
          val n = c.value
          ((t +| overheads.readout) *| n, n)
        case _ =>
          (TimeSpan.Zero, 0)

    val scienceDigest =
      SequenceDigest(
        ObserveClass.Science,
        CategorizedTime(ChargeClass.Program -> exposureTotal),
        SortedSet.empty,
        NonNegInt.unsafeFrom(0),
        state
      )

    ExecutionDigest(
      SetupTime(overheads.setup, TimeSpan.Zero),
      NonNegInt.unsafeFrom(if count > 0 then 1 else 0),
      SequenceDigest.Zero.copy(executionState = state),
      scienceDigest
    )

 /**
  * The time estimate for alien visitors cannot be calculated, we return the
  * total requested time instead
  */
  def alienDigest(
    totalRequestTime: Option[TimeSpan],
    state:            ExecutionState
  ): ExecutionDigest =
    val total = totalRequestTime.getOrElse(TimeSpan.Zero)
    val scienceDigest =
      SequenceDigest(
        ObserveClass.Science,
        CategorizedTime(ChargeClass.Program -> total),
        SortedSet.empty,
        NonNegInt.unsafeFrom(0),
        state
      )

    ExecutionDigest(
      SetupTime.Zero, // no info about setup time
      NonNegInt.unsafeFrom(0),
      SequenceDigest.Zero.copy(executionState = state),
      scienceDigest
    )
