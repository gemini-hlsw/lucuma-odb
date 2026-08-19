// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.itc.Conversions
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

class LegacyITCGmosR150BlueSuite extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 15.minutes

  def brightness(value: BigDecimal) =
    BrightnessValue
      .unsafeFrom(value)
      .withUnit[VegaMagnitude]
      .toMeasureTagged

  lazy val target = TargetData(
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        Some(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V)),
        SortedMap(
          Band.SloanG -> brightness(14.52),
          Band.SloanR -> brightness(13.8),
          Band.SloanI -> brightness(13.66),
          Band.B      -> brightness(14.99),
          Band.V      -> brightness(14.09),
          Band.R      -> brightness(13.97),
          Band.J      -> brightness(12.246),
          Band.H      -> brightness(11.845),
          Band.K      -> brightness(11.68)
        )
      )
    ),
    RadialVelocity.fromMetersPerSecond
      .getOption(BigDecimal(6423))
      .flatMap(_.toRedshift)
      .get
  )

  lazy val mode = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(540).get,
    GmosNorthGrating.R150_G5308,
    GmosNorthFpuParam(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_1_00)),
    None,
    Some(
      GmosCcdMode(
        GmosXBinning.Four,
        GmosYBinning.Two,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )
    ),
    Some(GmosRoi.FullFrame),
    PortDisposition.Side
  )

  lazy val conditions = ItcObservingConditions(
    ImageQuality.Preset.OnePointZero.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.PointThree.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Wet,
    SkyBackground.Bright,
    2.0
  )

  lazy val exposureTimeMode = ExposureTimeMode.SignalToNoiseMode(
    SignalToNoise.unsafeFromBigDecimalExact(100.0),
    Wavelength.decimalNanometers.getOption(600).get
  )

  // Initialize the local ITC
  lazy val localItc = {
    val jarFiles =
      new File("itc/service/ocslib")
        .getAbsoluteFile()
        .listFiles(new FileFilter() {
          override def accept(file: File): Boolean =
            file.getName().endsWith(".jar")
        })
    LocalItc[IO](
      new ReverseClassLoader(jarFiles.map(_.toURI.toURL), ClassLoader.getSystemClassLoader())
    )
  }

  // GMOS R150 below ~608 nm reports an x-axis that reaches past 0 nm on the blue CCD.
  // That used to fail the whole calculation with "Wavelength <= 0 received in ITC graph data."
  test("R150 at a blue central wavelength decodes".tag(LegacyITCTest)):
    val (params, _) = toItcParameters(target, mode, conditions, exposureTimeMode)

    localItc
      .calculateGraphs(params.asJson.noSpaces)
      .map: result =>
        val graphs = result.fold(e => fail(s"legacy ITC failed: ${e.mkString(", ")}"), identity)

        val series = graphs.groups.toList.flatMap(_.graphs.toList).flatMap(_.series)
        assert(series.nonEmpty)

        series.foreach: s =>
          assert(s.xAxis.start > 0, s"'${s.title}' starts at ${s.xAxis.start} nm")
          assertEquals(s.dataY.length, s.xAxis.count, s"'${s.title}' data does not match its axis")
          assertEquals(s.xAxis.wavelengthAt(0).isDefined,
                       true,
                       s"'${s.title}' has no first wavelength"
          )

  // The trim shifts the x-axis, so make sure the surviving samples still line up with their
  // wavelengths: the S/N reported at 600 nm must match what the legacy CCDs report.
  test("R150 at a blue central wavelength keeps the S/N".tag(LegacyITCTest)):
    val (params, _) = toItcParameters(target, mode, conditions, exposureTimeMode)

    localItc
      .calculateGraphs(params.asJson.noSpaces)
      .map: result =>
        val remote = result.fold(e => fail(s"legacy ITC failed: ${e.mkString(", ")}"), identity)
        val graphs =
          Conversions.targetGraphsFromLegacy(remote.ccds, remote.groups, exposureTimeMode.at)

        val atWavelength = graphs.atWavelengthFinalSNRatio
          .getOrElse(fail("no final S/N at 600 nm"))
          .value
          .toBigDecimal

        // The legacy CCDs report a total S/N of ~107 for this configuration
        assert(atWavelength > 100 && atWavelength < 115, s"S/N at 600 nm is $atWavelength")
        assert(graphs.peakFinalSNRatio.value.toBigDecimal >= atWavelength)
