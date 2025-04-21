// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.odb.service.ItcService

case class Obscalc(
  observationId:    Observation.Id,
  state:            Obscalc.State,
  lastInvalidation: Timestamp,
  lastUpdate:       Timestamp,
  retryAt:          Option[Timestamp],
  failureCount:     NonNegInt,
  result:           Option[Obscalc.Result]
)

object Obscalc:

  case class PendingCalc(
    programId:        Program.Id,
    observationId:    Observation.Id,
    lastInvalidation: Timestamp
  )

  enum State(val tag: String) derives Enumerated:
    case Pending     extends State("pending")
    case Retry       extends State("retry")
    case Calculating extends State("calculating")
    case Ready       extends State("ready")

  case class ItcResult(
    imaging:      ItcService.TargetResult,
    spectroscopy: ItcService.TargetResult
  )

  sealed trait Result extends Product with Serializable:
    def fold[A](
      error:         Result.Error         => A,
      withoutTarget: Result.WithoutTarget => A,
      withTarget:    Result.WithTarget    => A
    ): A =
      this match
        case a@Result.Error(_)         => error(a)
        case a@Result.WithoutTarget(_) => withoutTarget(a)
        case a@Result.WithTarget(_, _) => withTarget(a)

    def odbError: Option[OdbError] =
      fold(_.e.some, _ => none, _ => none)

    def itcResult: Option[ItcResult] =
      fold(_ => none, _ => none, _.i.some)

    def digest: Option[ExecutionDigest] =
      fold(_ => none, _.d.some, _.d.some)

  object Result:
    case class Error(e: OdbError)                           extends Result
    case class WithoutTarget(d: ExecutionDigest)            extends Result
    case class WithTarget(i: ItcResult, d: ExecutionDigest) extends Result