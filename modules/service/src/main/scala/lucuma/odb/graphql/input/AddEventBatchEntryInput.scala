// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.binding.*

enum AddEventBatchEntryInput:
  case Dataset(value: AddDatasetEventInput)
  case Sequence(value: AddSequenceEventInput)
  case Slew(value: AddSlewEventInput)
  case Step(value: AddStepEventInput)

  def clientTime: Option[Timestamp] =
    this match
      case Dataset(v)  => v.clientTime
      case Sequence(v) => v.clientTime
      case Slew(v)     => v.clientTime
      case Step(v)     => v.clientTime

  def idempotencyKey: Option[IdempotencyKey] =
    this match
      case Dataset(v)  => v.idempotencyKey
      case Sequence(v) => v.idempotencyKey
      case Slew(v)     => v.idempotencyKey
      case Step(v)     => v.idempotencyKey

object AddEventBatchEntryInput:

  val Binding: Matcher[AddEventBatchEntryInput] =
    ObjectFieldsBinding.rmap:
      case List(
        AddDatasetEventInput.Binding.Option("dataset", rDataset),
        AddSequenceEventInput.Binding.Option("sequence", rSequence),
        AddSlewEventInput.Binding.Option("slew", rSlew),
        AddStepEventInput.Binding.Option("step", rStep)
      ) =>
        (rDataset, rSequence, rSlew, rStep).parTupled.flatMap: (d, sq, sl, st) =>
          val chosen =
            d.map(Dataset(_)).toList   ++
            sq.map(Sequence(_)).toList ++
            sl.map(Slew(_)).toList     ++
            st.map(Step(_)).toList
          chosen match
            case one :: Nil => requireComplete(one)
            case Nil        => Result.failure("An AddEventBatchEntryInput must set exactly one event; none were set.")
            case _          => Result.failure("An AddEventBatchEntryInput must set exactly one event; more than one was set.")

  // A batched event, unlike a singular one, must carry both its own client time
  // (its recorded time would otherwise collapse onto the batch transaction's) and
  // an idempotency key (so the batch can be retried safely).  Report both
  // requirements at once so a caller missing both needn't fix them one at a time.
  private def requireComplete(e: AddEventBatchEntryInput): Result[AddEventBatchEntryInput] =
    if e.clientTime.isEmpty || e.idempotencyKey.isEmpty then
      Result.failure("Each event in a batch must supply both a 'clientTime' and an 'idempotencyKey'.")
    else
      Result.success(e)
