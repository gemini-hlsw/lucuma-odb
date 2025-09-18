// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.benchmarks

import cats.effect.*
import cats.effect.unsafe.implicits.global
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.concurrent.TimeUnit

case class BenchmarkState(
  itc:               Itc[IO],
  gmosNorthMode:     ObservingMode.SpectroscopyMode,
  mediumTarget:      TargetData,
  grayConditions:    ItcObservingConditions,
  testWavelength:    Wavelength,
  testSignalToNoise: SignalToNoise,
  testExposureTime:  TimeSpan,
  testExposureCount: PosInt
)

object BenchmarkState:
  def fromTestData(testData: ItcTestData): BenchmarkState =
    BenchmarkState(
      itc = testData.itc,
      gmosNorthMode = testData.gmosNorthMode,
      mediumTarget = testData.mediumTarget,
      grayConditions = testData.grayConditions,
      testWavelength = testData.testWavelength,
      testSignalToNoise = testData.testSignalToNoise,
      testExposureTime = testData.testExposureTime,
      testExposureCount = testData.testExposureCount
    )

/**
 * JMH benchmarks for core ITC calculations. Focuses on the three main calculation methods without
 * GraphQL/Redis overhead.
 *
 * Run with: sbt "benchmark/Jmh/run .*ItcCoreBenchmark.*"
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Fork(value = 1, jvmArgs = Array("-Xms1g", "-Xmx2g"))
@Warmup(iterations = 3, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
class ItcCoreBenchmark:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  var state: Option[BenchmarkState] = None

  @Setup
  def setup(): Unit =
    val testData = ItcBenchmarkSetup.initializeTestData().unsafeRunSync()
    state = Some(BenchmarkState.fromTestData(testData))

  @Benchmark
  def benchmarkIntegrationTime_GmosN_Medium(bh: Blackhole): Unit =
    val s      = state.getOrElse(sys.exit(1))
    val result = s.itc
      .calculateIntegrationTime(
        s.mediumTarget,
        s.testWavelength,
        s.gmosNorthMode,
        s.grayConditions,
        s.testSignalToNoise
      )
      .unsafeRunSync()
    bh.consume(result)

  @Benchmark
  def benchmarkSignalToNoise_GmosN_Medium(bh: Blackhole): Unit =
    val s      = state.getOrElse(sys.exit(1))
    val result = s.itc
      .calculateSignalToNoise(
        s.mediumTarget,
        s.testWavelength,
        s.gmosNorthMode,
        s.grayConditions,
        s.testExposureTime,
        s.testExposureCount
      )
      .unsafeRunSync()
    bh.consume(result)

  @Benchmark
  def benchmarkGraphs_GmosN_Medium(bh: Blackhole): Unit =
    val s      = state.getOrElse(sys.exit(1))
    val result = s.itc
      .calculateGraphs(
        s.mediumTarget,
        s.testWavelength,
        s.gmosNorthMode,
        s.grayConditions,
        s.testExposureTime,
        s.testExposureCount
      )
      .unsafeRunSync()
    bh.consume(result)
