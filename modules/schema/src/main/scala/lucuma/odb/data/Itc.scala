// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.data.NonEmptyList
import eu.timepit.refined.cats.*
import lucuma.core.data.Zipper
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt

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

  enum ModeDataType(val tag: String) derives Enumerated:
    case Empty            extends ModeDataType("empty")
    case GmosNorthImaging extends ModeDataType("gmos_north_imaging")
    case GmosSouthImaging extends ModeDataType("gmos_south_imaging")

  sealed trait ModeData:
    def dataType: ModeDataType =
      this match
        case ModeData.Empty                    => ModeDataType.Empty
        case ModeData.GmosNorthImaging(filter) => ModeDataType.GmosNorthImaging
        case ModeData.GmosSouthImaging(filter) => ModeDataType.GmosSouthImaging

  object ModeData:
    case object Empty                                    extends ModeData
    case class GmosNorthImaging(filter: GmosNorthFilter) extends ModeData
    case class GmosSouthImaging(filter: GmosSouthFilter) extends ModeData

  // Corresponds to ItcResultSet in the OdbSchema.graphql
  case class ResultSet(
    modeData: ModeData,
    results:  Zipper[Result]
  )

  private def targetIds(z: Zipper[Result]): NonEmptyList[Target.Id] =
    z.toNel.map(_.targetId).sortBy(_.value)

  def fromResults(
    modeData:    ModeData,
    acquisition: Option[Zipper[Result]],
    science:     Zipper[Result]
  ): Option[Itc] =
    Option.when(acquisition.forall(z => targetIds(z) === targetIds(science)))(
      Itc(acquisition.map(ResultSet(modeData, _)), ResultSet(modeData, science))
    )