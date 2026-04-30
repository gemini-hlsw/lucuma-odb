// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
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

// X-axis values are always wavelength in nanometers
case class ItcXAxis(start: Double, end: Double, count: Int) derives Decoder, Encoder.AsObject:
  assert(start >= 0, "Wavelength <= 0 received in ITC graph data.")

  val step: Double = (end - start) / (count - 1)

  def at(index: Int): Double                       = start + index * step
  def wavelengthAt(index: Int): Option[Wavelength] =
    Wavelength.intPicometers
      .getOption((at(index) * 1000).toInt)

  // Find the index for which the wavelength is at or just above the given wavelength. Return None if out of range.
  // It might be more accurate to use 'round` instead of 'ceil' but 'ceil' matches previous behavior.
  def indexOf(w: Double): Option[Int]     =
    if (w < start || w > end) none
    else ((w - start) / step).ceil.toInt.some
  def indexOf(w: Wavelength): Option[Int] =
    indexOf(w.toNanometers.value.value.toDouble)

case class ItcYAxis(min: Double, indexOfMin: Int, max: Double, indexOfMax: Int)
    derives Decoder,
      Encoder.AsObject
object ItcYAxis:
  def fromData(data: NonEmptyList[Double]): ItcYAxis =
    val (minTuple, maxTuple, _) = data.foldLeft(((Double.MaxValue, 0), (Double.MinValue, 0), 0)) {
      case ((min, max, count), y) =>
        val newMin = if (y < min._1) (y, count) else min
        val newMax = if (y > max._1) (y, count) else max
        (newMin, newMax, count + 1)
    }
    ItcYAxis(minTuple._1, minTuple._2, maxTuple._1, maxTuple._2)

case class ItcSeries(
  title:      String,
  seriesType: SeriesDataType,
  dataY:      NonEmptyList[Double],
  xAxis:      ItcXAxis,
  yAxis:      ItcYAxis
) derives Encoder.AsObject:
  def wavelengthAtMaxAndMax: Option[(Wavelength, Double)] =
    xAxis.wavelengthAt(yAxis.indexOfMax).tupleRight(yAxis.max)

  def yValueAtWavelength(w: Wavelength): Option[Double] =
    xAxis.indexOf(w).flatMap(i => dataY.toList.lift(i))

object ItcSeries:
  def apply(
    title:      String,
    seriesType: SeriesDataType,
    dataY:      NonEmptyList[Double],
    xAxis:      ItcXAxis
  ): ItcSeries =
    ItcSeries(title, seriesType, dataY, xAxis, ItcYAxis.fromData(dataY))

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
