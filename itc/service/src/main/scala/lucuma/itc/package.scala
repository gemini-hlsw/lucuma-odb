// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.ItcRemoteCcd

case class UpstreamException(msg: List[String]) extends RuntimeException(msg.mkString("\n"))

enum SNResultType(val tag: String) derives Enumerated:
  case Success          extends SNResultType("success")
  case SourceTooBright  extends SNResultType("source_too_bright")
  case BelowRange       extends SNResultType("below_range")
  case AboveRange       extends SNResultType("above_range")
  case NoData           extends SNResultType("no_data")
  case CalculationError extends SNResultType("calculation_error")

sealed trait SNCalcResult:
  def resultType: SNResultType

object SNCalcResult:
  given (using Encoder[Wavelength]): Encoder[SNCalcResult] = Encoder.instance { a =>
    Json
      .obj(("resultType", a.resultType.asJson))
      .deepMerge(a match {
        case s @ SNCalcSuccess(_)          => s.asJson
        case NoData()                      => Json.Null
        case w @ WavelengthAtAboveRange(_) => w.asJson
        case w @ WavelengthAtBelowRange(_) => w.asJson
        case _                             => Json.Null
      })
  }

  case class SNCalcSuccess(signalToNoise: SignalToNoise) extends SNCalcResult
      derives Encoder.AsObject:
    val resultType = SNResultType.Success

  case class NoData() extends SNCalcResult:
    val resultType = SNResultType.NoData

  case class WavelengthAtBelowRange(signalToNoiseAt: Wavelength) extends SNCalcResult:
    val resultType = SNResultType.BelowRange

  object WavelengthAtBelowRange:
    given (using Encoder[Wavelength]): Encoder[WavelengthAtBelowRange] = Encoder.AsObject.derived

  case class WavelengthAtAboveRange(signalToNoiseAt: Wavelength) extends SNCalcResult:
    val resultType = SNResultType.AboveRange

  object WavelengthAtAboveRange:
    given (using Encoder[Wavelength]): Encoder[WavelengthAtAboveRange] = Encoder.AsObject.derived

  /** Generic calculation error */
  case class CalculationError(msg: String) extends SNCalcResult derives Encoder.AsObject:
    val resultType = SNResultType.CalculationError

object Conversions:
  def targetGraphsFromLegacy(
    ccds:           NonEmptyChain[ItcRemoteCcd],
    originalGraphs: NonEmptyChain[ItcGraphGroup],
    atWavelength:   Wavelength
  ): TargetGraphs = {
    val graphs: NonEmptyChain[ItcGraphGroup] =
      originalGraphs.map: graph =>
        graph.copy(graphs = graph.graphs.map: c =>
          c.copy(series = c.series.map: s =>
            s.copy(dataY = s.dataY.map(y => if y.isNaN then 0.0 else y))))

    // Find the wavelength that gives the maximum value for the given series data type
    // It returns one value per ccd, returns a pair of wavelength and value
    def wavelengthAtMaxSN(
      graph:          ItcGraph,
      seriesDataType: SeriesDataType
    ): List[(Wavelength, Double)] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .map(_.wavelengthAtMaxAndMax)
        .flattenOption

    // The S/N at the requested wavelength. More than one series can cover that wavelength — the
    // two-slit IFU's blue and red slits both do — so report the best of them rather than whichever
    // happens to come first. That matches how each CCD's peak is taken below, and the slit the
    // legacy exposure time solve optimises for, which is likewise the highest S/N there.
    def signalToNoiseAtWv(graph: ItcGraph, seriesDataType: SeriesDataType): Option[SignalToNoise] =
      graph.series
        .filter(_.seriesType === seriesDataType)
        .map(_.yValueAtWavelength(atWavelength))
        .flattenOption
        .maximumOption
        .flatMap(v => SignalToNoise.FromBigDecimalRounding.getOption(v))

    // Calculate the wavelengths at where the peaks happen.
    //
    // Most modes report one S/N series per CCD, and each CCD takes the series at its own
    // index. Two shapes report several series per CCD, and both order them with the CCD
    // varying slowest — all of CCD 0's series, then all of CCD 1's — so each CCD takes the
    // peak over its own contiguous run of them:
    //   - GNIRS cross-dispersed reports a single CCD with one series per spectral order, so
    //     that run is the whole list and the one CCD aggregates the peak across every order.
    //   - The GMOS two-slit IFU reports a blue and a red slit series per CCD, so each CCD
    //     takes the better of its own pair. The two slits see the same target through
    //     slightly shifted wavelength windows, so the higher of the two is the peak for
    //     that CCD.
    //
    // Only those two shapes have a surplus of series. Measured against the ocslib jars, this
    // is every spectroscopy mode that reaches here — recheck it after a jar refresh, since
    // the counts are upstream behaviour and do change (cross-dispersed gained its
    // single-exposure series that way):
    //
    //   mode                 ccds  series per graph  pairing
    //   GMOS longslit          3          3          by index
    //   GMOS one-slit IFU      3          3          by index
    //   GMOS two-slit IFU      3          6          2 per CCD
    //   GHOST                  2          1          by index, one group per detector
    //   IGRINS-2               2          1          by index, one group per detector
    //   Flamingos 2            1          1          by index
    //   GNIRS longslit         1          1          by index
    //   GNIRS cross-dispersed  1          6          6 per CCD, i.e. the whole list
    val calculatedCCDs: Chain[ItcCcd] =
      graphs
        .flatMap(_.graphs)
        .filter(_.graphType === GraphType.S2NGraph)
        .flatMap: graph =>
          val finalSN  = wavelengthAtMaxSN(graph, SeriesDataType.FinalS2NData)
          val singleSN = wavelengthAtMaxSN(graph, SeriesDataType.SingleS2NData)

          // Select the (wavelength, value) peak for the CCD at the given index. A surplus of
          // series is taken as one run per CCD only when it divides evenly; a surplus that
          // does not is left to pair by index, since without knowing its layout there is
          // nothing better to do than what we did before.
          val ccdCount: Int = ccds.length.toInt

          def peakFor(series: List[(Wavelength, Double)], i: Int): Option[(Wavelength, Double)] =
            val seriesPerCcd: Int = series.size / ccdCount
            if series.sizeIs > ccdCount && series.size % ccdCount === 0 then
              series.slice(i * seriesPerCcd, (i + 1) * seriesPerCcd).maxByOption(_._2)
            else series.lift(i)

          ccds.zipWithIndex
            .map: (ccd, i) =>
              val finalPeak  = peakFor(finalSN, i)
              val singlePeak = peakFor(singleSN, i)

              finalPeak.flatMap: (maxFinalAt, maxFinalValue) =>
                for
                  single <- SignalToNoise.FromBigDecimalRounding.getOption(ccd.singleSNRatio)
                  total  <- SignalToNoise.FromBigDecimalRounding.getOption(ccd.totalSNRatio)
                yield ItcCcd(
                  SingleSN(single),
                  singlePeak.map(_._2),
                  TotalSN(total),
                  Some(maxFinalValue),
                  Some(maxFinalAt),
                  singlePeak.map(_._1),
                  ccd.peakPixelFlux,
                  ccd.wellDepth,
                  ccd.ampGain,
                  ccd.warnings
                )
            .toChain
            .flattenOption

    val maxTotalSNRatio: Option[Double]  =
      calculatedCCDs
        .map(_.maxTotalSNRatio)
        .maximumOption
        .flatten
    val peakFinalSNRatio: SignalToNoise  = maxTotalSNRatio
      .flatMap(SignalToNoise.FromBigDecimalRounding.getOption(_))
      .getOrElse(throw UpstreamException(List("Peak Total SN is not available")))
    val maxSingleSNRatio: Option[Double] = calculatedCCDs
      .map(_.maxSingleSNRatio)
      .maximumOption
      .flatten
    val peakSingleSNRatio: SignalToNoise = maxSingleSNRatio
      .flatMap(SignalToNoise.FromBigDecimalRounding.getOption(_))
      // A mode that reports no single-exposure S/N series leaves no peak to take; fall back
      // to the per-CCD single S/N reported by the ITC rather than failing the request.
      .orElse(calculatedCCDs.map(_.singleSNRatio.value).maximumOption)
      .getOrElse(throw UpstreamException(List("Peak Single SN is not available")))

    // Picks between graph groups, not series; only methods that return one series are supported (individual; sum).
    def wvAtRatio(seriesType: SeriesDataType): Option[SignalToNoise] =
      graphs
        .flatMap(_.graphs)
        .filter(_.graphType === GraphType.S2NGraph)
        .map(c => signalToNoiseAtWv(c, seriesType))
        .collect { case Some(v) => v }
        .headOption

    val wvAtFinalRatio: Option[SignalToNoise]  = wvAtRatio(SeriesDataType.FinalS2NData)
    val wvAtSingleRatio: Option[SignalToNoise] = wvAtRatio(SeriesDataType.SingleS2NData)

    assert(calculatedCCDs.length === ccds.length.toInt)
    TargetGraphs(
      NonEmptyChain.fromChainUnsafe(calculatedCCDs),
      graphs.flatMap(_.graphs),
      TotalSN(peakFinalSNRatio),
      wvAtFinalRatio.map(TotalSN.apply(_)),
      SingleSN(peakSingleSNRatio),
      wvAtSingleRatio.map(SingleSN.apply(_))
    )
  }
