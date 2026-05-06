// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.data.Zipper
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.util.NewType
import lucuma.itc.Error
import lucuma.itc.GraphType
import lucuma.itc.ItcCcd
import lucuma.itc.ItcVersions
import lucuma.itc.ItcXAxis
import lucuma.itc.ItcYAxis
import lucuma.itc.SeriesDataType
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.json.decoders.given

// These are limited versions of the graph for the client as we don't want to transfer all the data.
// This cascades into new types for all wrapping types.
case class SeriesResult(
  title:      String,
  seriesType: SeriesDataType,
  dataY:      NonEmptyList[Double],
  xAxis:      ItcXAxis,
  yAxis:      ItcYAxis
) derives Eq,
      Decoder

case class GraphResult(graphType: GraphType, series: List[SeriesResult]) derives Eq, Decoder

case class TargetGraphs(
  ccds:                      NonEmptyChain[ItcCcd],
  graphData:                 NonEmptyChain[GraphResult],
  peakFinalSNRatio:          TotalSN,
  atWavelengthFinalSNRatio:  Option[TotalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
) derives Eq,
      Decoder

case class TargetTimeAndGraphsResult(
  integrationTime: TargetIntegrationTime,
  graphs:          TargetGraphs
) derives Decoder:
  export integrationTime.{ccds as integrationTimeCcds, *}
  export graphs.{ccds as graphCcds, *}

object TargetTimeAndGraphsResult:
  given Order[TargetTimeAndGraphsResult] = Order.by(_.integrationTime)

object TargetTimeAndGraphsResultOutcome extends NewType[Either[Error, TargetTimeAndGraphsResult]]:
  given Decoder[TargetTimeAndGraphsResultOutcome] =
    Decoder[TargetTimeAndGraphsResult]
      .map(_.asRight)
      .or(Decoder[Error].map(_.asLeft))
      .map(TargetTimeAndGraphsResultOutcome(_))
type TargetTimeAndGraphsResultOutcome = TargetTimeAndGraphsResultOutcome.Type

object AsterismTimeAndGraphsResult extends NewType[NonEmptyChain[TargetTimeAndGraphsResultOutcome]]:
  extension (a: AsterismTimeAndGraphsResult)
    // If there are no errors, return an Zipper with the brightest target focused.
    private def onlyIfNoErrors: Option[Zipper[TargetTimeAndGraphsResult]] =
      a.value
        .traverse(_.value.toOption)
        .map: nec =>
          Zipper.of(nec.head, nec.tail.toList*).focusMin

    // The brightest target is the one with the shortest exposure time.
    // Only returns the index of the brightest target if there are no errors.
    def brightestIndex: Option[Int] =
      onlyIfNoErrors.map(_.indexOfFocus)

    def brightest: Option[TargetTimeAndGraphsResult] =
      onlyIfNoErrors.map(_.focus)
type AsterismTimeAndGraphsResult = AsterismTimeAndGraphsResult.Type

case class SpectroscopyIntegrationTimeAndGraphsResult(
  versions:       ItcVersions,
  graphs:         AsterismTimeAndGraphsResult,
  brightestIndex: Option[Int]
) derives Eq
