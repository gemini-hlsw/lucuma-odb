// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
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
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.util.TimeSpan
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData

import scala.collection.immutable.SortedMap

// sc-11586: imaging selects the brightness by filter wavelength, not by the exposure time mode's `at`.
class ImagingBrightnessSelectionSuite extends munit.FunSuite:

  private def brightness(value: BigDecimal) =
    BrightnessValue.unsafeFrom(value).withUnit[VegaMagnitude].toMeasureTagged

  private def target(bands: Band*): TargetData =
    TargetData(
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.B5_7V).some,
          SortedMap.from(bands.map(_ -> brightness(15)))
        )
      ),
      RadialVelocity.fromMetersPerSecond.getOption(BigDecimal(0)).flatMap(_.toRedshift).get
    )

  private val conditions = ItcObservingConditions(
    ImageQuality.Preset.OnePointZero.toImageQuality.toArcSeconds,
    CloudExtinction.Preset.PointThree.toCloudExtinction.toVegaMagnitude,
    WaterVapor.Wet,
    SkyBackground.Bright,
    2.0
  )

  private def pm(value: Int): Wavelength =
    Wavelength.intPicometers.getOption(value).get

  private def nm(value: Int): Wavelength =
    Wavelength.decimalNanometers.getOption(BigDecimal(value)).get

  private def signalToNoise(at: Wavelength): ExposureTimeMode =
    ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(100.0), at)

  private def timeAndCount(at: Wavelength): ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(TimeSpan.fromSeconds(5).get, PosInt.unsafeFrom(10), at)

  private val gnirsH = ObservingMode.ImagingMode.Gnirs(
    GnirsFilter.Order4,
    GnirsCamera.ShortBlue,
    GnirsReadMode.Bright,
    GnirsWellDepth.Shallow,
    PosInt.unsafeFrom(1),
    PortDisposition.Side
  )

  private def selected(
    t:    TargetData,
    mode: ObservingMode,
    etm:  ExposureTimeMode
  ): Either[Band, Wavelength] =
    toItcParameters(t, mode, conditions, etm)._2

  private val modes: List[(String, ObservingMode.ImagingMode, Band)] =
    List(
      ("GMOS North r",
       ObservingMode.ImagingMode.GmosNorth(GmosNorthFilter.RPrime, none, PortDisposition.Side),
       Band.SloanR
      ),
      ("GMOS South r",
       ObservingMode.ImagingMode.GmosSouth(GmosSouthFilter.RPrime, none, PortDisposition.Side),
       Band.SloanR
      ),
      ("Flamingos-2 H",
       ObservingMode.ImagingMode.Flamingos2(Flamingos2Filter.H,
                                            Flamingos2ReadMode.Bright,
                                            PortDisposition.Side
       ),
       Band.H
      ),
      ("GNIRS H", gnirsH, Band.H),
      ("GNIRS H2", gnirsH.copy(filter = GnirsFilter.H2), Band.K)
    )

  // 1 pm is what o-11118 stored; 5 µm pulls the other way.
  private val ats: List[Wavelength] = List(pm(1), nm(5000))

  modes.foreach: (name, mode, expected) =>
    test(s"$name selects $expected whatever `at` says"):
      val t = target(Band.U, Band.SloanR, Band.H, Band.K, Band.M)
      ats.foreach: at =>
        assertEquals(selected(t, mode, signalToNoise(at)), expected.asLeft)
        assertEquals(selected(t, mode, timeAndCount(at)), expected.asLeft)

  test("the selection does not depend on the other bands present"):
    val etm = signalToNoise(pm(1))
    assertEquals(selected(target(Band.H), gnirsH, etm), Band.H.asLeft)
    assertEquals(selected(target(Band.U, Band.H), gnirsH, etm), Band.H.asLeft)
    assertEquals(selected(target(Band.U, Band.H, Band.M), gnirsH, etm), Band.H.asLeft)

  test("the trace and error wavelength is the filter's for imaging"):
    assertEquals(selectionWavelength(gnirsH, signalToNoise(pm(1))),
                 GnirsFilter.Order4.centralWavelength
    )

  test("spectroscopy still selects by `at`"):
    val mode = ObservingMode.SpectroscopyMode.GmosNorth(
      nm(540),
      GmosNorthGrating.B480_G5309,
      GmosNorthFpuParam(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50)),
      None,
      None,
      Some(GmosRoi.FullFrame),
      PortDisposition.Side,
      ifuAnalysis = none
    )
    val t    = target(Band.U, Band.SloanR, Band.H)
    assertEquals(selected(t, mode, signalToNoise(nm(360))), Band.U.asLeft)
    assertEquals(selected(t, mode, signalToNoise(nm(1600))), Band.H.asLeft)
