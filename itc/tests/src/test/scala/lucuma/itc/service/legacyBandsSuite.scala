// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import coulomb.syntax.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Enumerated
import lucuma.core.util.tag
import lucuma.itc.input.TargetDataInput
import lucuma.itc.legacy.LegacyBands
import lucuma.itc.service.requests.*

import scala.collection.immutable.SortedMap

class legacyBandsSuite extends munit.FunSuite:

  private val GaiaMessage =
    "The ITC does not support GAIA bands. At least one non-GAIA brightness measure is required."

  private def brightness(value: BigDecimal) =
    BrightnessValue.unsafeFrom(value).withUnit[VegaMagnitude].toMeasureTagged

  private def surfaceBrightness(value: BigDecimal) =
    BrightnessValue.unsafeFrom(value).withUnit[VegaMagnitudePerArcsec2].toMeasureTagged

  private val sed =
    UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.B5_7V).some

  private def point(brightnesses: (Band, BrightnessMeasure[Integrated])*): SourceProfile =
    SourceProfile.Point(SpectralDefinition.BandNormalized(sed, SortedMap.from(brightnesses)))

  private def uniform(brightnesses: (Band, BrightnessMeasure[Surface])*): SourceProfile =
    SourceProfile.Uniform(SpectralDefinition.BandNormalized(sed, SortedMap.from(brightnesses)))

  private val radialVelocity =
    RadialVelocity.fromMetersPerSecond.getOption(BigDecimal(0)).get

  private def toData(sourceProfile: SourceProfile) =
    List(TargetDataInput(sourceProfile, radialVelocity)).targetInputsToData

  private val gaiaBands = Set[Band](Band.Gaia, Band.GaiaBP, Band.GaiaRP)

  test("the unsupported bands are exactly the Gaia ones") {
    assertEquals(Enumerated[Band].all.toSet -- LegacyBands.Supported, gaiaBands)
  }

  test("a Gaia band is not selected over a supported one") {
    // The failure reported in sc-10351: at 600nm, Gaia's 641nm center beats V's 550nm by 41nm
    // to 50nm.
    val profile = point(
      Band.B    -> brightness(8.85),
      Band.V    -> brightness(8.82),
      Band.J    -> brightness(8.599),
      Band.H    -> brightness(8.649),
      Band.K    -> brightness(8.671),
      Band.Gaia -> brightness(8.776856)
    )

    assertEquals(
      SourceProfile.integratedBrightnesses.getOption(profile).map(_.keySet.toList),
      List(Band.B, Band.V, Band.J, Band.H, Band.K, Band.Gaia).some
    )

    toData(profile) match
      case Result.Success(ts) =>
        assertEquals(ts.head.bandOrLine(Wavelength.fromIntNanometers(600).get), Band.V.asLeft)
      case other              => fail(s"Expected success, got $other")
  }

  test("Gaia bands are stripped from a uniform profile too") {
    val profile = uniform(
      Band.V      -> surfaceBrightness(8.82),
      Band.GaiaBP -> surfaceBrightness(8.9),
      Band.GaiaRP -> surfaceBrightness(8.7)
    )

    toData(profile) match
      case Result.Success(ts) =>
        assertEquals(
          SourceProfile.surfaceBrightnesses.getOption(ts.head.sourceProfile).map(_.keySet.toList),
          List(Band.V).some
        )
      case other              => fail(s"Expected success, got $other")
  }

  test("a Gaia-only target is rejected with a message naming GAIA") {
    toData(point(Band.Gaia -> brightness(8.776856))) match
      case Result.Failure(ps) => assertEquals(ps.head.message, GaiaMessage)
      case other              => fail(s"Expected failure, got $other")
  }

  test("a target with no brightnesses at all is rejected without naming GAIA") {
    toData(point()) match
      case Result.Failure(ps) =>
        assertEquals(ps.head.message, "No brightness measures provided for target.")
      case other              => fail(s"Expected failure, got $other")
  }

  test("an emission lines profile is unaffected") {
    val wavelength = Wavelength.fromIntNanometers(600).get
    val profile    = SourceProfile.Point(
      SpectralDefinition.EmissionLines[Integrated](
        SortedMap(
          wavelength -> EmissionLine(
            LineWidthValue.unsafeFrom(BigDecimal(1.0)).withUnit[KilometersPerSecond],
            Measure(
              LineFluxValue.unsafeFrom(BigDecimal(0.5)),
              TaggedUnit[WattsPerMeter2, LineFlux[Integrated]].unit
            ).tag
          )
        ),
        Measure(
          FluxDensityContinuumValue.unsafeFrom(BigDecimal(0.5)),
          TaggedUnit[WattsPerMeter2Micrometer, FluxDensityContinuum[Integrated]].unit
        ).tag
      )
    )

    toData(profile) match
      case Result.Success(ts) => assertEquals(ts.head.sourceProfile, profile)
      case other              => fail(s"Expected success, got $other")
  }
