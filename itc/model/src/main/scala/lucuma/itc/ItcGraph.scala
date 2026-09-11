// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import scala.collection.immutable.ArraySeq

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
  val step: Double = (end - start) / (count - 1)

  // Drop the first n samples, keeping the sample spacing and the remaining wavelengths intact.
  def drop(n: Int): ItcXAxis = ItcXAxis(start + n * step, end, count - n)

  // Number of leading samples that are at or below 0 nm, i.e. have no valid Wavelength.
  def nonPositiveCount: Int =
    if start > 0 then 0 else (((-start) / step).floor.toInt + 1).min(count)

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

/**
 * Codecs for the unboxed sample arrays held by `ItcSeries`.
 *
 * A chart series runs to a couple of thousand samples, and the previous `NonEmptyList[Double]` cost
 * a cons cell plus a boxed `Double` for every one of them. These instances keep the samples in a
 * primitive array on both sides of the wire: `decodeArray` fills an `Array[Double]` in a single
 * pass, and the encoder wraps the array in place rather than copying it into a `List`. Circe still
 * boxes each element as it renders, which is unavoidable.
 *
 * Kept in an object so importers can target them: they are instances for a very general type.
 */
object iarray:
  given decodeIArrayDouble: Decoder[IArray[Double]] =
    Decoder.decodeArray[Double].map(IArray.unsafeFromArray)

  given encodeIArrayDouble: Encoder[IArray[Double]] =
    Encoder
      .encodeSeq[Double]
      .contramap(a => ArraySeq.unsafeWrapArray(a.asInstanceOf[Array[Double]]))

  // Only ItcGraph's derived Eq needs this, which is test-only; a while loop would avoid the
  // wrapped-array boxing if it ever reaches a hot path.
  given eqIArrayDouble: Eq[IArray[Double]] = Eq.instance(_.sameElements(_))

import iarray.given

case class ItcYAxis(min: Double, indexOfMin: Int, max: Double, indexOfMax: Int)
    derives Decoder,
      Encoder.AsObject
object ItcYAxis:
  // Indexed loop rather than a fold: this runs over every sample of every series and the fold
  // allocated a tuple per element.
  def fromData(data: IArray[Double]): ItcYAxis =
    var min      = Double.MaxValue
    var minIndex = 0
    var max      = Double.MinValue
    var maxIndex = 0
    var i        = 0
    while i < data.length do
      val y = data(i)
      if y < min then
        min = y
        minIndex = i
      if y > max then
        max = y
        maxIndex = i
      i += 1
    ItcYAxis(min, minIndex, max, maxIndex)

case class ItcSeries(
  title:      String,
  seriesType: SeriesDataType,
  dataY:      IArray[Double],
  xAxis:      ItcXAxis,
  yAxis:      ItcYAxis
) derives Encoder.AsObject:
  def wavelengthAtMaxAndMax: Option[(Wavelength, Double)] =
    xAxis.wavelengthAt(yAxis.indexOfMax).tupleRight(yAxis.max)

  def yValueAtWavelength(w: Wavelength): Option[Double] =
    xAxis.indexOf(w).filter(i => i >= 0 && i < dataY.length).map(dataY.apply)

object ItcSeries:
  /**
   * Returns None if `dataY` is empty, since ItcYAxis is not meaningful without at least one sample.
   * The case class constructor is still public, so this is a convention rather than an invariant:
   * callers building a series from raw samples should prefer `from`.
   */
  def from(
    title:      String,
    seriesType: SeriesDataType,
    dataY:      IArray[Double],
    xAxis:      ItcXAxis
  ): Option[ItcSeries] =
    Option.when(dataY.nonEmpty)(
      ItcSeries(title, seriesType, dataY, xAxis, ItcYAxis.fromData(dataY))
    )

  /**
   * Build a series out of legacy data, dropping the samples at or below 0 nm and trimming the
   * title.
   *
   * Low dispersion gratings at blue central wavelengths (e.g. GMOS R150 below ~608 nm) report an
   * x-axis that extends past 0 nm.
   *
   * Those samples are zero padding but have no valid Wavelength, so they are dropped together with
   * their y-values to keep the index to wavelength mapping intact. Returns None if no sample is
   * above 0 nm.
   *
   * Legacy titles append the CCD name to the series name, and that name is empty for the first CCD,
   * so those arrive padded ("Blue Slit Signal "). Multi-CCD GMOS graphs are only comparable across
   * graph types if the same series is named identically in each, hence the trim.
   */
  def fromLegacy(
    title:      String,
    seriesType: SeriesDataType,
    dataY:      IArray[Double],
    xAxis:      ItcXAxis
  ): Option[ItcSeries] =
    val trimmedTitle = title.trim
    xAxis.nonPositiveCount match
      case 0 => ItcSeries.from(trimmedTitle, seriesType, dataY, xAxis)
      case n => ItcSeries.from(trimmedTitle, seriesType, dataY.drop(n), xAxis.drop(n))

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
