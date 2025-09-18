// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.benchmarks

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Using

/**
 * Simple test for ITC core calculations.
 */
object ItcSpeedTest:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  case class PerformanceResult(
    testName:     String,
    avgTimeMs:    Double,
    minTimeMs:    Double,
    maxTimeMs:    Double,
    iterations:   Int,
    memoryUsedMB: Double
  )

  def runPerformanceTests(): IO[List[PerformanceResult]] =
    for
      testData    <- ItcBenchmarkSetup.initializeTestData()
      _           <- IO.println("Running performance tests with 5 iterations per test...")
      itResult    <- timeCalculation("Integration Time - GMOS-N", 5) {
                       testData.itc.calculateIntegrationTime(testData.mediumTarget,
                                                             testData.testWavelength,
                                                             testData.gmosNorthMode,
                                                             testData.grayConditions,
                                                             testData.testSignalToNoise
                       )
                     }
      snResult    <- timeCalculation("Signal-to-Noise - GMOS-N", 5) {
                       testData.itc.calculateSignalToNoise(testData.mediumTarget,
                                                           testData.testWavelength,
                                                           testData.gmosNorthMode,
                                                           testData.grayConditions,
                                                           testData.testExposureTime,
                                                           testData.testExposureCount
                       )
                     }
      graphResult <- timeCalculation("Graph Generation - GMOS-N", 5) {
                       testData.itc.calculateGraphs(testData.mediumTarget,
                                                    testData.testWavelength,
                                                    testData.gmosNorthMode,
                                                    testData.grayConditions,
                                                    testData.testExposureTime,
                                                    testData.testExposureCount
                       )
                     }
    yield List(itResult, snResult, graphResult)

  def timeCalculation[T](testName: String, iterations: Int)(
    calculation: => IO[T]
  ): IO[PerformanceResult] =
    for {
      // Warmup
      _         <- (1 to 2).toList.traverse(_ => calculation.attempt)
      // memory before
      memBefore <- IO(memory())
      // calculate
      timings   <- (1 to iterations).toList.traverse { _ =>
                     for
                       start <- IO.realTime
                       _     <- calculation
                       end   <- IO.realTime
                     yield (end - start).toMillis.toDouble
                   }
      // memory after
      memAfter  <- IO(memory())
      memUsed    = memAfter - memBefore
      avgTime    = timings.sum / timings.length
      minTime    = timings.min
      maxTime    = timings.max
      _         <-
        IO.println(
          f"$testName%-30s: ${avgTime}%6.1f ms avg (${minTime}%6.1f - ${maxTime}%6.1f ms) [${memUsed}%4.1f MB]"
        )
    } yield PerformanceResult(testName, avgTime, minTime, maxTime, iterations, memUsed)

  def memory(): Double =
    val runtime   = Runtime.getRuntime
    val usedBytes = runtime.totalMemory() - runtime.freeMemory()
    usedBytes / (1024.0 * 1024.0)

  def printResultsTable(results: List[PerformanceResult]): Unit =
    println(
      f"${"Test Name"}%-30s ${"Avg (ms)"}%8s ${"Min (ms)"}%8s ${"Max (ms)"}%8s ${"Memory (MB)"}%10s"
    )
    println("-" * 70)
    results.foreach { result =>
      println(
        f"${result.testName}%-30s ${result.avgTimeMs}%8.1f ${result.minTimeMs}%8.1f ${result.maxTimeMs}%8.1f ${result.memoryUsedMB}%10.1f"
      )
    }

  def saveResultsToCSV(results: List[PerformanceResult]): Unit =
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
    val filename  = s"itc-performance-results-$timestamp.csv"

    Using(new PrintWriter(filename)) { writer =>
      writer.println("TestName,AvgTimeMs,MinTimeMs,MaxTimeMs,Iterations,MemoryUsedMB")
      results.foreach { result =>
        writer.println(
          s"${result.testName},${result.avgTimeMs},${result.minTimeMs},${result.maxTimeMs},${result.iterations},${result.memoryUsedMB}"
        )
      }
    }

    println(s"\nDetailed results saved to: $filename")

  def main(args: Array[String]): Unit = {
    val results = runPerformanceTests().unsafeRunSync()

    println("=============================")
    println("Test results:")

    printResultsTable(results)
    saveResultsToCSV(results)

    println("=============================")
  }
