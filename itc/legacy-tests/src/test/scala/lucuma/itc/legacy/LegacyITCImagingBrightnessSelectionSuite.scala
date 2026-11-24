// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.IO
import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.Main.ReverseClassLoader
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import java.io.File
import java.io.FileFilter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

/**
 * The configuration of o-11118, the report behind sc-11586: GNIRS H imaging of a target with both U
 * and H brightnesses, whose exposure time mode sits at 1 pm. Selecting by that wavelength picked U
 * and gave wrong times or an out of bounds failure.
 */
class LegacyITCImagingBrightnessSelectionSuite extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 15.minutes

  private def brightness(value: BigDecimal) =
    BrightnessValue.unsafeFrom(value).withUnit[VegaMagnitude].toMeasureTagged

  private def target(brightnesses: (Band, BigDecimal)*): TargetData =
    TargetData(
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.PowerLaw(BigDecimal(0)).some,
          SortedMap.from(brightnesses.map((b, v) => b -> brightness(v)))
        )
      ),
      RadialVelocity.fromMetersPerSecond.getOption(BigDecimal(0)).flatMap(_.toRedshift).get
    )

  private val mode = ObservingMode.ImagingMode.Gnirs(
    GnirsFilter.Order4,
    GnirsCamera.ShortBlue,
    GnirsReadMode.Bright,
    GnirsWellDepth.Shallow,
    PosInt.unsafeFrom(1),
    PortDisposition.Side
  )

  private val conditions = ItcObservingConditions(
    ImageQuality.Preset.OnePointZero.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.PointThree.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Wet,
    SkyBackground.Bright,
    2.0
  )

  private def signalToNoise(value: BigDecimal): ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(
      SignalToNoise.unsafeFromBigDecimalExact(value),
      Wavelength.intPicometers.getOption(1).get
    )

  private lazy val localItc = {
    val jarFiles =
      new File("itc/service/ocslib")
        .getAbsoluteFile()
        .listFiles(new FileFilter() {
          override def accept(file: File): Boolean = file.getName().endsWith(".jar")
        })
    LocalItc[IO](
      new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())
    )
  }

  // (exposure seconds, frame count) of the selected detector, with the band it was normalized to.
  private def calculate(
    t:  TargetData,
    sn: BigDecimal
  ): IO[(Either[Band, Wavelength], (Double, Int))] =
    val (params, bandOrLine) = toItcParameters(t, mode, conditions, signalToNoise(sn))
    localItc
      .calculate(params.asJson.noSpaces)
      .flatMap: r =>
        IO.fromEither(r.leftMap(e => new RuntimeException(e.toString)))
      .map: r =>
        val c = r.exposureCalculation
        val d = c.detectors.toList(c.selectedIndex)
        (bandOrLine, (d.exposureTime, d.frameCount.value))

  private val hOnly = target(Band.H -> 17)

  test("H only: 19 x 5s".tag(LegacyITCTest)):
    calculate(hOnly, 100).assertEquals((Band.H.asLeft, (5.0, 19)))

  test("H and U gives the H only result".tag(LegacyITCTest)):
    calculate(target(Band.U -> 12, Band.H -> 17), 100).assertEquals((Band.H.asLeft, (5.0, 19)))

  test("other bands do not change the result".tag(LegacyITCTest)):
    val variants = List(
      target(Band.U -> 20, Band.H -> 17),
      target(Band.U -> 5, Band.H  -> 17),
      target(Band.V -> 10, Band.H -> 17, Band.M -> 3)
    )
    calculate(hOnly, 100).flatMap: expected =>
      variants
        .traverse(calculate(_, 100))
        .map: results =>
          results.foreach(assertEquals(_, expected))

  test("acquisition signal to noise no longer fails".tag(LegacyITCTest)):
    (calculate(hOnly, 20), calculate(target(Band.U -> 12, Band.H -> 17), 20)).mapN: (a, b) =>
      assertEquals(b, a)
