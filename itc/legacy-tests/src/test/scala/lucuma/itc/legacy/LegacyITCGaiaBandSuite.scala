// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.IO
import cats.syntax.all.*
import coulomb.syntax.*
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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.Main.ReverseClassLoader
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import java.io.File
import java.io.FileFilter
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

/**
 * Pins the premise behind `lucuma.itc.legacy.LegacyBands`: the legacy OCS ITC cannot decode a
 * `normBand` of "G", which is what `Band.Gaia` encodes to. If a future `update_itc_jars.sh` brings
 * Gaia support, the first test here fails and `LegacyBands.Supported` should grow to match.
 *
 * The configuration is the one from sc-10351, the error report that prompted the filter.
 */
class LegacyITCGaiaBandSuite extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 15.minutes

  private def brightness(value: BigDecimal) =
    BrightnessValue.unsafeFrom(value).withUnit[VegaMagnitude].toMeasureTagged

  private lazy val target = TargetData(
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.B5_7V).some,
        SortedMap(
          Band.B    -> brightness(8.85),
          Band.V    -> brightness(8.82),
          Band.J    -> brightness(8.599),
          Band.H    -> brightness(8.649),
          Band.K    -> brightness(8.671),
          Band.Gaia -> brightness(8.776856)
        )
      )
    ),
    RadialVelocity.fromMetersPerSecond.getOption(BigDecimal(0)).flatMap(_.toRedshift).get
  )

  private lazy val mode = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(540).get,
    GmosNorthGrating.B480_G5309,
    GmosNorthFpuParam(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50)),
    None,
    Some(
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.Two,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )
    ),
    Some(GmosRoi.FullFrame),
    PortDisposition.Side,
    ifuAnalysis = none
  )

  private lazy val conditions = ItcObservingConditions(
    ImageQuality.Preset.OnePointZero.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.PointThree.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Wet,
    SkyBackground.Bright,
    2.0
  )

  private lazy val exposureTimeMode = ExposureTimeMode.SignalToNoiseMode(
    SignalToNoise.unsafeFromBigDecimalExact(100.0),
    Wavelength.decimalNanometers.getOption(600).get
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

  private def calculate(band: Band) =
    val params = ItcParameters(
      source = ItcSourceDefinition(target, band.asLeft),
      observation = toItcParameters(target, mode, conditions, exposureTimeMode)._1.observation,
      conditions = conditions,
      telescope = ItcTelescopeDetails(ItcWavefrontSensor.OIWFS, mode.portDisposition),
      instrument = ItcInstrumentDetails(mode)
    )
    localItc.calculate(params.asJson.noSpaces)

  test("the legacy ITC rejects a Gaia normBand".tag(LegacyITCTest)):
    calculate(Band.Gaia).map: result =>
      assert(result.isLeft, s"legacy ITC unexpectedly accepted normBand 'G': $result")

  test("the legacy ITC accepts the same target normalized on V".tag(LegacyITCTest)):
    calculate(Band.V).map: result =>
      assert(result.isRight, s"legacy ITC failed on normBand 'V': $result")
