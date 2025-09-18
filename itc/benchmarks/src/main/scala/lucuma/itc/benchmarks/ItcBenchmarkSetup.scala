// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.benchmarks

import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.TaggedUnit
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.TimeSpan
import lucuma.core.util.tag
import lucuma.itc.legacy.FLocalItc
import lucuma.itc.legacy.ItcImpl
import lucuma.itc.legacy.LocalItc
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.Itc
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import natchez.Trace.Implicits.noop
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.File
import java.net.URL
import java.net.URLClassLoader

/**
 * Shared test data for ITC benchmarks
 */
case class ItcTestData(
  itc:               Itc[IO],
  testWavelength:    Wavelength,
  testSignalToNoise: SignalToNoise,
  testExposureTime:  TimeSpan,
  testExposureCount: PosInt,
  gmosNorthMode:     ObservingMode.SpectroscopyMode,
  grayConditions:    ItcObservingConditions,
  mediumTarget:      TargetData
)

object ItcBenchmarkSetup:

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  def findProjectRoot: IO[File] = IO {
    Option(System.getProperty("project.root"))
      .map(new File(_))
      .filter(_.exists())
      .getOrElse {
        // Fallback: detect if we're in itc/benchmarks and navigate up
        val userDir     = System.getProperty("user.dir")
        val projectRoot = if (userDir.endsWith("itc/benchmarks")) {
          new File(userDir).getParentFile.getParentFile.getAbsolutePath
        } else {
          userDir
        }
        new File(projectRoot)
      }
  }

  private def setupItc: IO[Itc[IO]] =
    findProjectRoot.flatMap: projectRoot =>
      IO {
        val ocslibPath = new File(projectRoot, "itc/service/ocslib").getAbsolutePath
        val ocslibDir  = new File(ocslibPath)

        if (!ocslibDir.exists())
          throw new RuntimeException(s"ocslib directory not found at: $ocslibPath")

        val jarFiles = Option(ocslibDir.listFiles()).getOrElse(Array.empty[File])

        val jarUrls = jarFiles
          .filter(_.getName.endsWith(".jar"))
          .map(_.toURI.toURL)
          .toArray[URL]

        val classLoader = new URLClassLoader(jarUrls, null)
        val localItc    = LocalItc(classLoader)
        val fLocalItc   = FLocalItc[IO](localItc)

        ItcImpl.build[IO](fLocalItc)
      }

  private def createTestData = {
    val testWavelength    = Wavelength.fromIntNanometers(600).get
    val testSignalToNoise = SignalToNoise.unsafeFromBigDecimalExact(10.0)
    val testExposureTime  = TimeSpan.fromSeconds(2.0).get
    val testExposureCount = PosInt.unsafeFrom(1)

    val gmosNorthMode = ObservingMode.SpectroscopyMode.GmosNorth(
      Wavelength.fromIntNanometers(600).get,
      GmosNorthGrating.B1200_G5301,
      GmosNorthFpuParam(GmosNorthFpu.LongSlit_1_00),
      None,
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.High,
        GmosAmpReadMode.Fast
      ).some,
      GmosRoi.FullFrame.some
    )

    val conditions = ItcObservingConditions(
      ImageQuality.Preset.PointEight.toImageQuality.toArcSeconds,
      CloudExtinction.Preset.OnePointZero.toCloudExtinction.toVegaMagnitude,
      WaterVapor.Median,
      SkyBackground.Gray,
      1.0
    )

    val sourceProfile = SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        Some(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V)),
        scala.collection.immutable.SortedMap(
          Band.R -> Measure(
            BrightnessValue.unsafeFrom(BigDecimal(12.0)),
            TaggedUnit[VegaMagnitude, Brightness[Integrated]].unit
          ).tag
        )
      )
    )

    val target = TargetData(sourceProfile, Redshift(0.03))

    (testWavelength,
     testSignalToNoise,
     testExposureTime,
     testExposureCount,
     gmosNorthMode,
     conditions,
     target
    )
  }

  def initializeTestData(): IO[ItcTestData] =
    setupItc.map: itc =>
      val (wavelength, signalToNoise, exposureTime, exposureCount, gmosMode, conditions, target) =
        createTestData
      ItcTestData(
        itc = itc,
        testWavelength = wavelength,
        testSignalToNoise = signalToNoise,
        testExposureTime = exposureTime,
        testExposureCount = exposureCount,
        gmosNorthMode = gmosMode,
        grayConditions = conditions,
        mediumTarget = target
      )
