// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.util.NewType
import lucuma.itc.*
import lucuma.itc.client.json.decoders.given

// These are limited versions of the graph for the client as we don't want to transfer all the data.
// This cascades into new types for all wrapping types.
case class SeriesResult(
  title:      String,
  seriesType: SeriesDataType,
  dataY:      List[Double],
  xAxis:      Option[ItcAxis],
  yAxis:      Option[ItcAxis]
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

case class TargetGraphsResult(
  graphs: TargetGraphs,
  band:   Band
) derives Eq,
      Decoder

object TargetGraphsResultOutcome extends NewType[Either[Error, TargetGraphsResult]]:
  given Decoder[TargetGraphsResultOutcome] =
    Decoder[TargetGraphsResult]
      .map(_.asRight)
      .or(Decoder[Error].map(_.asLeft))
      .map(TargetGraphsResultOutcome(_))
type TargetGraphsResultOutcome = TargetGraphsResultOutcome.Type

object AsterismTargetGraphsResultOutcomes extends NewType[NonEmptyChain[TargetGraphsResultOutcome]]:
  extension (a: AsterismTargetGraphsResultOutcomes)
    def collectErrors: Option[NonEmptyChain[(Error, Int)]] =
      NonEmptyChain.fromChain:
        a.value.map(_.value).zipWithIndex.collect { case (Left(e), i) => (e, i) }
type AsterismTargetGraphsResultOutcomes = AsterismTargetGraphsResultOutcomes.Type

case class SpectroscopyGraphsResult(
  versions:     ItcVersions,
  targetGraphs: AsterismTargetGraphsResultOutcomes
) derives Eq,
      Decoder
