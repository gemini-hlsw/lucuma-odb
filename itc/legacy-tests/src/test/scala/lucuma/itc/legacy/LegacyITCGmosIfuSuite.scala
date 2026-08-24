// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.GmosSouthFpuParam
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*

/**
 * End-to-end checks of GMOS IFU against the legacy recipe, running whatever
 * `ObservingMode.analysisMethod` produces rather than a hand-written analysis method. Getting the
 * sampling geometry wrong is silent: an element offset a few arcsec off target comes back with a
 * S/N of exactly zero, and a radius below one lenslet pitch comes back with no apertures at all, so
 * "it returned a result" is not enough on its own.
 */
class LegacyITCGmosIfuSuite extends CommonITCLegacySuite:

  private val wv: Wavelength = Wavelength.decimalNanometers.getOption(600).get

  override val obs: ItcObservationDetails = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      exposureCount = 10,
      exposureDuration = 30.seconds,
      wavelengthAt = wv,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  // The legacy recipe rejects spatial binning other than 1 for IFU observations.
  private val ccdMode: Option[GmosCcdMode] = GmosCcdMode(
    GmosXBinning.One,
    GmosYBinning.One,
    GmosAmpCount.Twelve,
    GmosAmpGain.Low,
    GmosAmpReadMode.Slow
  ).some

  private def gnMode(fpu: GmosNorthFpu): ObservingMode.SpectroscopyMode.GmosNorth =
    ObservingMode.SpectroscopyMode.GmosNorth(
      wv,
      GmosNorthGrating.R831_G5302,
      GmosNorthFpuParam(GmosFpuMask.Builtin(fpu)),
      none,
      ccdMode,
      GmosRoi.FullFrame.some,
      PortDisposition.Side,
      none
    )

  private def gsMode(fpu: GmosSouthFpu): ObservingMode.SpectroscopyMode.GmosSouth =
    ObservingMode.SpectroscopyMode.GmosSouth(
      wv,
      GmosSouthGrating.R831_G5322,
      GmosSouthFpuParam(GmosFpuMask.Builtin(fpu)),
      none,
      ccdMode,
      GmosRoi.FullFrame.some,
      PortDisposition.Side,
      none
    )

  val instrument: ItcInstrumentDetails = ItcInstrumentDetails(gnMode(GmosNorthFpu.Ifu2Slits))

  private def run(mode: ObservingMode): IO[IntegrationTimeRemoteResult] =
    localItc
      .calculate(bodyConf(sourceDefinition, obs, mode, mode.analysisMethod).asJson.noSpaces)
      .map:
        case Left(errs) => fail(s"${mode.description}: ${errs.mkString("; ")}")
        case Right(r)   => r

  private def runWith(
    mode:     ObservingMode,
    analysis: ItcObservationDetails.AnalysisMethod
  ): IO[IntegrationTimeRemoteResult] =
    localItc
      .calculate(bodyConf(sourceDefinition, obs, mode, analysis).asJson.noSpaces)
      .map:
        case Left(errs) => fail(s"${mode.description}: ${errs.mkString("; ")}")
        case Right(r)   => r

  private def arcsec(v: BigDecimal): Angle =
    Angle.signedDecimalArcseconds.reverseGet(v)

  private def assertRealSignal(mode: ObservingMode): IO[Unit] =
    run(mode).map: r =>
      r.ccds.toList.zipWithIndex.foreach: (ccd, i) =>
        assert(ccd.singleSNRatio > 0,
               s"${mode.description} CCD $i single S/N was ${ccd.singleSNRatio}"
        )
        assert(ccd.totalSNRatio > 0,
               s"${mode.description} CCD $i total S/N was ${ccd.totalSNRatio}"
        )

  test("gmos north ifu produces signal".tag(LegacyITCTest)):
    List(GmosNorthFpu.Ifu2Slits, GmosNorthFpu.IfuBlue, GmosNorthFpu.IfuRed)
      .traverse_(f => assertRealSignal(gnMode(f)))

  test("gmos south ifu produces signal".tag(LegacyITCTest)):
    List(GmosSouthFpu.Ifu2Slits, GmosSouthFpu.IfuBlue, GmosSouthFpu.IfuRed)
      .traverse_(f => assertRealSignal(gsMode(f)))

  // Requires the OCS patch that reports `signalToNoiseAt` from the IFU branch of GmosRecipe;
  // before it, the legacy recipe returned None here while longslit returned a value.
  test("gmos ifu reports the S/N at the requested wavelength".tag(LegacyITCTest)):
    List(GmosNorthFpu.Ifu2Slits, GmosNorthFpu.IfuBlue).traverse_ { f =>
      run(gnMode(f)).map: r =>
        assertEquals(r.signalToNoiseAt.map(_.wavelength), wv.some, s"for $f")
    }

  // The default radius is one lenslet pitch, which encloses only the central element, so it must
  // agree with measuring that single element directly. The boundary is exact: at 0.2" the six
  // neighbours sit at 0.2" and 0.2016" and are excluded by a strict `<`, so a drift in the
  // recipe's geometry constants would silently pull them in and inflate the S/N by ~2.4x.
  test("gmos ifu default radius encloses exactly the central element".tag(LegacyITCTest)):
    List(GmosNorthFpu.Ifu2Slits, GmosNorthFpu.IfuBlue).traverse_ { f =>
      for
        dflt   <- run(gnMode(f))
        single <- runWith(gnMode(f),
                          ItcObservationDetails.AnalysisMethod.Ifu
                            .Single(gnMode(f).ifuSky.get.fibres.value, 0.0)
                  )
      yield
        assertEqualsDouble(dflt.ccds.head.singleSNRatio,
                           single.ccds.head.singleSNRatio,
                           1e-9,
                           s"$f"
        )
        assertEqualsDouble(dflt.ccds.head.totalSNRatio, single.ccds.head.totalSNRatio, 1e-9, s"$f")
    }

  // Widening the radius pulls in more fibres, which must raise the S/N until the added noise
  // starts to dominate. A radius that never changed the answer would mean it is being dropped.
  test("gmos ifu summation radius changes the result".tag(LegacyITCTest)):
    val mode = gnMode(GmosNorthFpu.Ifu2Slits)
    for
      narrow <- run(mode)
      wider  <- run(mode.copy(ifuAnalysis = GmosIfuAnalysis.Sum(arcsec(0.5)).some))
    yield assert(
      wider.ccds.head.totalSNRatio > narrow.ccds.head.totalSNRatio,
      s"0.5\" gave ${wider.ccds.head.totalSNRatio}, default gave ${narrow.ccds.head.totalSNRatio}"
    )
