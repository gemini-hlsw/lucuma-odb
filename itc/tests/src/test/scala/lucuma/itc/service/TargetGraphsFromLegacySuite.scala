// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.itc.Conversions
import lucuma.itc.GraphType
import lucuma.itc.ItcGraph
import lucuma.itc.ItcGraphGroup
import lucuma.itc.ItcSeries
import lucuma.itc.ItcXAxis
import lucuma.itc.SeriesDataType
import lucuma.itc.TargetGraphs
import lucuma.itc.legacy.ItcRemoteCcd

class TargetGraphsFromLegacySuite extends munit.FunSuite:

  private val at: Wavelength = Wavelength.fromIntNanometers(1001).get

  private def finalSeries(order: Int, peak: Double): ItcSeries =
    ItcSeries(
      s"Final S/N Order ${order + 3}",
      SeriesDataType.FinalS2NData,
      NonEmptyList.of(0.0, peak),
      ItcXAxis(1000.0, 1001.0, 2)
    )

  private def singleSeries(order: Int, peak: Double): ItcSeries =
    ItcSeries(
      s"Single S/N Order ${order + 3}",
      SeriesDataType.SingleS2NData,
      NonEmptyList.of(0.0, peak),
      ItcXAxis(1000.0, 1001.0, 2)
    )

  private def ccd(single: Double, total: Double): ItcRemoteCcd =
    ItcRemoteCcd(single, total, 3.0, 4.0, 5.0, Nil)

  // GNIRS cross-dispersed: a single CCD, multiple Final S/N series (one per order),
  // and no single-exposure S/N series at all. Used to throw "Peak Total SN is not
  // available" because every CCD was dropped for lacking a single-exposure series.
  test("GNIRS cross-dispersed (single CCD, many final series, no single series)") {
    val finalPeaks = List(10.0, 20.0, 100.0, 30.0, 40.0, 50.0)
    val graph      =
      ItcGraph(
        GraphType.S2NGraph,
        finalPeaks.zipWithIndex.map((p, i) => finalSeries(i, p))
      )

    val result =
      Conversions.targetGraphsFromLegacy(
        NonEmptyChain.one(ccd(0.0, 100.0)),
        NonEmptyChain.one(ItcGraphGroup(NonEmptyChain.one(graph))),
        at
      )

    // The single CCD aggregates the peak across all of its series.
    assertEquals(result.ccds.length.toInt, 1)
    assertEquals(result.peakFinalSNRatio, TotalSN(SignalToNoise.unsafeFromBigDecimalExact(100.0)))
    // No single-exposure S/N series: max single is absent, peak falls back to the CCD value.
    assertEquals(result.ccds.head.maxSingleSNRatio, None)
    assertEquals(result.ccds.head.wavelengthForMaxSingleSNRatio, None)
    assertEquals(result.peakSingleSNRatio, SingleSN(SignalToNoise.unsafeFromBigDecimalExact(0.0)))
  }

  // GNIRS cross-dispersed once OCS also emits a single-exposure S/N series per order:
  // the single CCD must aggregate the peak across all single series too.
  test("GNIRS cross-dispersed (single CCD, many final AND single series)") {
    val finalPeaks  = List(10.0, 20.0, 100.0, 30.0, 40.0, 50.0)
    val singlePeaks = List(5.0, 12.0, 60.0, 18.0, 24.0, 30.0)
    val graph       =
      ItcGraph(
        GraphType.S2NGraph,
        finalPeaks.zipWithIndex.map((p, i) => finalSeries(i, p)) ++
          singlePeaks.zipWithIndex.map((p, i) => singleSeries(i, p))
      )

    val result =
      Conversions.targetGraphsFromLegacy(
        NonEmptyChain.one(ccd(60.0, 100.0)),
        NonEmptyChain.one(ItcGraphGroup(NonEmptyChain.one(graph))),
        at
      )

    assertEquals(result.ccds.length.toInt, 1)
    assertEquals(result.ccds.head.maxTotalSNRatio, Some(100.0))
    assertEquals(result.ccds.head.maxSingleSNRatio, Some(60.0))
    assertEquals(result.peakFinalSNRatio, TotalSN(SignalToNoise.unsafeFromBigDecimalExact(100.0)))
    assertEquals(result.peakSingleSNRatio, SingleSN(SignalToNoise.unsafeFromBigDecimalExact(60.0)))
  }

  // Multi-CCD instruments (e.g. GMOS) keep their per-CCD index pairing of series.
  test("multi-CCD instrument pairs each CCD with its series by index") {
    val graph =
      ItcGraph(
        GraphType.S2NGraph,
        List(
          singleSeries(0, 5.0),
          finalSeries(0, 50.0),
          singleSeries(1, 7.0),
          finalSeries(1, 70.0)
        )
      )

    val result =
      Conversions.targetGraphsFromLegacy(
        NonEmptyChain.of(ccd(5.0, 50.0), ccd(7.0, 70.0)),
        NonEmptyChain.one(ItcGraphGroup(NonEmptyChain.one(graph))),
        at
      )

    assertEquals(result.ccds.length.toInt, 2)
    assertEquals(result.ccds.head.maxTotalSNRatio, Some(50.0))
    assertEquals(result.ccds.head.maxSingleSNRatio, Some(5.0))
    assertEquals(result.ccds.last.maxTotalSNRatio, Some(70.0))
    assertEquals(result.ccds.last.maxSingleSNRatio, Some(7.0))
    assertEquals(result.peakFinalSNRatio, TotalSN(SignalToNoise.unsafeFromBigDecimalExact(70.0)))
    assertEquals(result.peakSingleSNRatio, SingleSN(SignalToNoise.unsafeFromBigDecimalExact(7.0)))
  }

  private def slitSeries(slit: String, tpe: SeriesDataType, peak: Double): ItcSeries =
    ItcSeries(
      s"$slit Slit S/N",
      tpe,
      NonEmptyList.of(0.0, peak),
      ItcXAxis(1000.0, 1001.0, 2)
    )

  // The GMOS two-slit IFU reports a blue and a red slit series per CCD, so the series arrive
  // in CCD-major pairs. Pairing by plain index would give CCD 2 a CCD 0 series and never read
  // the last CCD's own data at all.
  test("GMOS two-slit IFU pairs each CCD with its blue/red series pair") {
    // per CCD: (blue, red) final peak
    val peaks: List[(Double, Double)] = List((10.0, 50.0), (12.0, 90.0), (11.0, 60.0))
    val graph: ItcGraph               =
      ItcGraph(
        GraphType.S2NGraph,
        peaks.flatMap: (blue, red) =>
          List(
            slitSeries("Blue", SeriesDataType.SingleS2NData, blue / 2),
            slitSeries("Blue", SeriesDataType.FinalS2NData, blue),
            slitSeries("Red", SeriesDataType.SingleS2NData, red / 2),
            slitSeries("Red", SeriesDataType.FinalS2NData, red)
          )
      )

    val result: TargetGraphs =
      Conversions.targetGraphsFromLegacy(
        NonEmptyChain.of(ccd(45.0, 90.0), ccd(45.0, 90.0), ccd(45.0, 90.0)),
        NonEmptyChain.one(ItcGraphGroup(NonEmptyChain.one(graph))),
        at
      )

    assertEquals(result.ccds.length.toInt, 3)
    assertEquals(result.ccds.toList.map(_.maxTotalSNRatio),
                 List(Some(50.0), Some(90.0), Some(60.0))
    )
    assertEquals(result.ccds.toList.map(_.maxSingleSNRatio),
                 List(Some(25.0), Some(45.0), Some(30.0))
    )
    assertEquals(result.peakFinalSNRatio, TotalSN(SignalToNoise.unsafeFromBigDecimalExact(90.0)))
    assertEquals(result.peakSingleSNRatio, SingleSN(SignalToNoise.unsafeFromBigDecimalExact(45.0)))

    // Both slits cover the requested wavelength, so the value there is the better of them and not
    // the blue slit merely for coming first.
    assertEquals(
      result.atWavelengthFinalSNRatio,
      TotalSN(SignalToNoise.unsafeFromBigDecimalExact(90.0)).some
    )
    assertEquals(
      result.atWavelengthSingleSNRatio,
      SingleSN(SignalToNoise.unsafeFromBigDecimalExact(45.0)).some
    )
  }

  private def slitSeriesAt(
    slit:    String,
    tpe:     SeriesDataType,
    atStart: Double,
    atEnd:   Double
  ): ItcSeries =
    ItcSeries(s"$slit Slit S/N", tpe, NonEmptyList.of(atStart, atEnd), ItcXAxis(1000.0, 1001.0, 2))

  // The peak and the value at the requested wavelength are found by separate code paths, and a
  // slit can win one without winning the other. Here blue peaks higher overall but red is higher
  // at the wavelength, so reporting blue's value there would be wrong in a way that agreeing
  // peaks would hide.
  test("two-slit IFU takes the at-wavelength value from the better slit, not the first") {
    val graph: ItcGraph =
      ItcGraph(
        GraphType.S2NGraph,
        List(
          slitSeriesAt("Blue", SeriesDataType.SingleS2NData, 50.0, 10.0),
          slitSeriesAt("Blue", SeriesDataType.FinalS2NData, 100.0, 20.0),
          slitSeriesAt("Red", SeriesDataType.SingleS2NData, 2.0, 40.0),
          slitSeriesAt("Red", SeriesDataType.FinalS2NData, 5.0, 80.0)
        )
      )

    val result: TargetGraphs =
      Conversions.targetGraphsFromLegacy(
        NonEmptyChain.one(ccd(50.0, 100.0)),
        NonEmptyChain.one(ItcGraphGroup(NonEmptyChain.one(graph))),
        at
      )

    // Peak: blue wins, at 1000 nm rather than at the requested wavelength.
    assertEquals(result.peakFinalSNRatio, TotalSN(SignalToNoise.unsafeFromBigDecimalExact(100.0)))
    assertEquals(result.peakSingleSNRatio, SingleSN(SignalToNoise.unsafeFromBigDecimalExact(50.0)))

    // At 1001 nm: red wins, even though blue is the first series of its type.
    assertEquals(result.atWavelengthFinalSNRatio,
                 TotalSN(SignalToNoise.unsafeFromBigDecimalExact(80.0)).some
    )
    assertEquals(result.atWavelengthSingleSNRatio,
                 SingleSN(SignalToNoise.unsafeFromBigDecimalExact(40.0)).some
    )
  }
