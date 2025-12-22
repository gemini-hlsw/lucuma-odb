// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.data.NonEmptyList
import eu.timepit.refined.cats.*
import lucuma.core.data.Zipper
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt

/*
type Itc {
  """
  The ITC result for the science part of the sequence
  """
  science: ItcResultSet!

  """
  The ITC result for the acquisition part of the sequence, if any.
  """
  acquisition: ItcResultSet
}

type ItcResultSet {
  selected: ItcResult!
  all: [ItcResult!]
  index: NonNegInt!
}

type ItcResult {
  targetId: TargetId!
  exposureTime: TimeSpan!
  exposureCount: NonNegInt!
  signalToNoiseAt: SignalToNoiseAt
}
*/

case class Itc(
  acquisition: Option[Itc.ResultSet],
  science:     Itc.ResultSet
)

object Itc:

  // Corresponds to ItcResult in the OdbSchema.graphql
  case class Result(
    targetId:      Target.Id,
    value:         IntegrationTime,
    signalToNoise: Option[SignalToNoiseAt]
  ):
    def totalTime: Option[TimeSpan] =
      val total = BigInt(value.exposureTime.toMicroseconds) * value.exposureCount.value
      Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten

  object Result:
    given Order[Result] =
      Order.by(s => (s.totalTime, s.targetId))

  // Corresponds to ItcResultSet in the OdbSchema.graphql
  case class ResultSet(
    results: Zipper[Result]
  )

  private def targetIds(z: Zipper[Result]): NonEmptyList[Target.Id] =
    z.toNel.map(_.targetId).sortBy(_.value)

  def fromResults(
    acquisition: Option[Zipper[Result]],
    science:     Zipper[Result]
  ): Option[Itc] =
    Option.when(acquisition.forall(z => targetIds(z) === targetIds(science)))(
      Itc(acquisition.map(ResultSet.apply), ResultSet(science))
    )