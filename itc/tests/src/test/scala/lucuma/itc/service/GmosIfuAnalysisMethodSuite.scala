// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.GmosIfuAnalysis
import lucuma.itc.service.ItcObservationDetails.AnalysisMethod

/**
 * The IFU sky fibre counts come from the OCS OT (`AnalysisMethodPanel.defaultMethod`) and set the
 * sky-subtraction noise, while the summation radius decides how many fibres are summed and so how
 * much source flux is enclosed. Both are silent if wrong — a bad radius or offset returns a
 * plausible number rather than an error — hence this suite.
 */
class GmosIfuAnalysisMethodSuite extends munit.FunSuite:

  private val at: Wavelength = Wavelength.fromIntNanometers(600).get

  private def arcsec(v: BigDecimal): Angle =
    Angle.signedDecimalArcseconds.reverseGet(v)

  private def north(
    fpu:      GmosFpuMask[GmosNorthFpu],
    analysis: Option[GmosIfuAnalysis] = none
  ): AnalysisMethod =
    ObservingMode.SpectroscopyMode
      .GmosNorth(
        at,
        GmosNorthGrating.R831_G5302,
        GmosNorthFpuParam(fpu),
        none,
        none,
        none,
        PortDisposition.Side,
        analysis
      )
      .analysisMethod

  private def south(
    fpu:      GmosFpuMask[GmosSouthFpu],
    analysis: Option[GmosIfuAnalysis] = none
  ): AnalysisMethod =
    ObservingMode.SpectroscopyMode
      .GmosSouth(
        at,
        GmosSouthGrating.R831_G5322,
        GmosSouthFpuParam(fpu),
        none,
        none,
        none,
        PortDisposition.Side,
        analysis
      )
      .analysisMethod

  private val longSlit: AnalysisMethod = AnalysisMethod.Aperture.Auto(skyAperture = 5.0)

  private def sum(skyFibres: Int, radius: Double, isIfu2: Boolean): AnalysisMethod =
    AnalysisMethod.Ifu.Sum(skyFibres = skyFibres, num = radius, isIfu2 = isIfu2)

  // A 0.2" radius is one lenslet pitch, which encloses only the element on the field centre.
  test("GMOS North defaults to summing one lenslet pitch"):
    assertEquals(north(GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits)), sum(500, 0.2, true))
    assertEquals(north(GmosFpuMask.Builtin(GmosNorthFpu.IfuBlue)), sum(250, 0.2, false))
    assertEquals(north(GmosFpuMask.Builtin(GmosNorthFpu.IfuRed)), sum(250, 0.2, false))

  test("GMOS South defaults to summing one lenslet pitch"):
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.Ifu2Slits)), sum(500, 0.2, true))
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.IfuBlue)), sum(250, 0.2, false))
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.IfuRed)), sum(250, 0.2, false))

  // Nod & shuffle nods the same fibres between object and sky, so it gets a single sky sample
  // rather than a whole block. Sending 250 or 500 here would overstate the S/N by ~40%. It is
  // also not the recipe's "two slit" unit: `Gmos.isIfu2()` tests for IFU_1 alone.
  test("GMOS South nod & shuffle IFU gets a single sky fibre and is not two-slit"):
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.IfuNS2Slits)), sum(1, 0.2, false))
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.IfuNSBlue)), sum(1, 0.2, false))
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.IfuNSRed)), sum(1, 0.2, false))

  test("an explicit summation radius is passed through in arcsec"):
    assertEquals(
      north(GmosFpuMask.Builtin(GmosNorthFpu.Ifu2Slits),
            GmosIfuAnalysis.Sum(arcsec(BigDecimal("0.5"))).some
      ),
      sum(500, 0.5, true)
    )
    assertEquals(
      south(GmosFpuMask.Builtin(GmosSouthFpu.IfuRed),
            GmosIfuAnalysis.Sum(arcsec(BigDecimal("2.25"))).some
      ),
      sum(250, 2.25, false)
    )

  // An offset is a position in the field, so it is signed: `Angle` is modular and a naive
  // conversion would turn -1.5" into nearly 360 degrees.
  test("a single element offset keeps its sign"):
    assertEquals(
      north(GmosFpuMask.Builtin(GmosNorthFpu.IfuBlue),
            GmosIfuAnalysis.Single(arcsec(BigDecimal("0.0"))).some
      ),
      AnalysisMethod.Ifu.Single(skyFibres = 250, offset = 0.0)
    )
    assertEquals(
      north(GmosFpuMask.Builtin(GmosNorthFpu.IfuBlue),
            GmosIfuAnalysis.Single(arcsec(BigDecimal("-1.5"))).some
      ),
      AnalysisMethod.Ifu.Single(skyFibres = 250, offset = -1.5)
    )

  test("non-IFU focal plane units keep the aperture method"):
    assertEquals(north(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_1_00)), longSlit)
    assertEquals(south(GmosFpuMask.Builtin(GmosSouthFpu.Ns2)), longSlit)
    assertEquals(south(GmosFpuMask.Custom(ToBeDefined, GmosCustomSlitWidth.CustomWidth_1_00)),
                 longSlit
    )

  // The request layer rejects this combination, so the mode should never see it; if one is built
  // directly the focal plane unit still decides, rather than an IFU method reaching the recipe.
  test("an IFU analysis on a non-IFU focal plane unit is ignored"):
    assertEquals(
      north(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_1_00),
            GmosIfuAnalysis.Sum(arcsec(BigDecimal("0.5"))).some
      ),
      longSlit
    )
