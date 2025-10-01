// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.data.Zipper
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.service.ItcService

/**
 * Data used in performing background observation calculations.
 */
object Obscalc:

  /**
   * A PendingCalc identifies an observation that needs to be updated.  The
   * last invalidation time is compared with the current last invalidation
   * time when the result is written in order to determine the resulting
   * calculation state (ready or pending).
   */
  final case class PendingCalc(
    programId:        Program.Id,
    observationId:    Observation.Id,
    lastInvalidation: Timestamp
  )

  /**
   * Metadata associated with an observation calculation.
   *
   * @param programId        program associated with the observation
   * @param observationId    identifies the observation to update
   * @param state            stage, or phase, of the calculation process
   * @param lastInvalidation the last time the observation was modified in such
   *                         a way that it might impact calculations
   * @param lastUpdate       the time at which a result was last written
   * @param retryAt          when in the Retry state, this is the time at which
   *                         another attempt at the calculation may be made.
   *                         Failure retries are backed off expontentially to
   *                         avoid flooding the ITC.
   * @param failureCount     the number of failed attempts to update the
   *                         observation that have been made since it was last
   *                         marked invalid
   */
  final case class Meta(
    programId:        Program.Id,
    observationId:    Observation.Id,
    state:            CalculationState,
    lastInvalidation: Timestamp,
    lastUpdate:       Timestamp,
    retryAt:          Option[Timestamp],
    failureCount:     NonNegInt
  )

  final case class ItcResult(
    imaging:      ItcService.TargetResult,
    spectroscopy: ItcService.TargetResult
  ):
    def toAsterismResults: Option[ItcService.AsterismResults] =
      ItcService.AsterismResults.fromResults(
        Zipper.one(imaging),
        Zipper.one(spectroscopy)
      )

  /**
   * Obscalc calculation results.
   */
  sealed trait Result extends Product with Serializable:
    def fold[A](
      error:         Result.Error         => A,
      withoutTarget: Result.WithoutTarget => A,
      withTarget:    Result.WithTarget    => A
    ): A =
      this match
        case a@Result.Error(_, _)         => error(a)
        case a@Result.WithoutTarget(_, _) => withoutTarget(a)
        case a@Result.WithTarget(_, _, _) => withTarget(a)

    def odbError: Option[OdbError] =
      fold(_.e.some, _ => none, _ => none)

    def itcResult: Option[ItcResult] =
      fold(_ => none, _ => none, _.i.some)

    def digest: Option[ExecutionDigest] =
      fold(_ => none, _.d.some, _.d.some)

    def workflow: ObservationWorkflow =
      fold(_.w, _.w, _.w)

  object Result:
    case class Error(e: OdbError, w: ObservationWorkflow)                           extends Result
    case class WithoutTarget(d: ExecutionDigest, w: ObservationWorkflow)            extends Result
    case class WithTarget(i: ItcResult, d: ExecutionDigest, w: ObservationWorkflow) extends Result

  /**
   * The Obscalc Entry pairs metadata with a (possibily missing, possibly
   * stale) computation result.
   */
  final case class Entry(
    meta:   Obscalc.Meta,
    result: Option[Obscalc.Result]
  )

object BlindOffset:
  case class PendingCalc(
    programId: Program.Id,
    observationId: Observation.Id,
    lastInvalidation: Timestamp,
    explicitBase: Option[Coordinates],
    observationTime: Option[Timestamp]
  )
