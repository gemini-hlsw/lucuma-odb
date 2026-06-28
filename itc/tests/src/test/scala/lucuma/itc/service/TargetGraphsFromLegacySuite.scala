// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
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
