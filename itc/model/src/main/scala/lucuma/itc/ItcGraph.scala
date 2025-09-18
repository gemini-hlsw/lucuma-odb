// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonObject
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

enum SeriesDataType(val tag: String) derives Enumerated:
  case SignalData     extends SeriesDataType("signal_data")
  case BackgroundData extends SeriesDataType("background_data")
  case SingleS2NData  extends SeriesDataType("single_s2_ndata")
  case FinalS2NData   extends SeriesDataType("final_s2_ndata")
  case PixSigData     extends SeriesDataType("pix_sig_data")
  case PixBackData    extends SeriesDataType("pix_back_data")

enum GraphType(val tag: String) derives Enumerated:
  case SignalGraph      extends GraphType("signal_graph")
  case SignalPixelGraph extends GraphType("signal_pixel_graph")
  case S2NGraph         extends GraphType("s2n_graph")

case class ItcAxis(start: Double, end: Double, min: Double, max: Double, count: Int)
    derives Decoder,
      Encoder.AsObject

object ItcAxis:
  // Calculate the values on the axis' range
  def calcAxis(data: List[(Double, Double)], fun: ((Double, Double)) => Double): Option[ItcAxis] =
    if (data.nonEmpty)
      val (min, max, count) =
        data.foldLeft((Double.MaxValue, Double.MinValue, 0)) { case ((max, min, count), current) =>
          val x = fun(current)
          (x.min(max), x.max(min), count + 1)
        }
      ItcAxis(fun(data.head), fun(data.last), min, max, count).some
    else none

case class ItcSeries private (
  title:      String,
  seriesType: SeriesDataType,
  data:       List[(Double, Double)],
  dataX:      List[Double],
  dataY:      List[Double],
  xAxis:      Option[ItcAxis],
  yAxis:      Option[ItcAxis]
) derives Encoder.AsObject

object ItcSeries:
  def apply(title: String, seriesType: SeriesDataType, data: List[(Double, Double)]): ItcSeries =
    ItcSeries(
      title,
      seriesType,
      data,
      data.map(_._1),
      data.map(_._2),
      ItcAxis.calcAxis(data, _._1),
      ItcAxis.calcAxis(data, _._2)
    )

case class ItcGraph(graphType: GraphType, series: List[ItcSeries]) derives Eq, Encoder.AsObject

case class ItcGraphGroup(graphs: NonEmptyChain[ItcGraph]) derives Eq, Encoder.AsObject

case class TargetGraphs(
  ccds:                      NonEmptyChain[ItcCcd],
  graphData:                 NonEmptyChain[ItcGraph],
  peakFinalSNRatio:          TotalSN,
  atWavelengthFinalSNRatio:  Option[TotalSN],
  peakSingleSNRatio:         SingleSN,
  atWavelengthSingleSNRatio: Option[SingleSN]
)

object TargetGraphs:
  given (using Encoder[ItcCcd]): Encoder[TargetGraphs] = deriveEncoder

case class TargetGraphsResult(
  graphs:     TargetGraphs,
  bandOrLine: Either[Band, Wavelength]
) derives Eq:
  export graphs.*

object TargetGraphsResult:
  given (using Encoder[Wavelength]): Encoder.AsObject[TargetGraphsResult] = x =>
    JsonObject(
      "graphs"       -> x.graphs.asJson,
      "band"         -> x.bandOrLine.left.toOption.asJson,
      "emissionLine" -> x.bandOrLine.toOption.asJson
    )

case class SpectroscopyGraphsResult(
  versions:     ItcVersions,
  targetGraphs: AsterismTargetGraphsOutcomes
) derives Eq

object SpectroscopyGraphsResult:
  given (using Encoder[AsterismTargetGraphsOutcomes]): Encoder.AsObject[SpectroscopyGraphsResult] =
    deriveEncoder
