// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig.given
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsDynamicConfig.given
import lucuma.core.syntax.timespan.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class SmartGcalRulesSuite extends ScalaCheckSuite:

  property("GMOS North: a custom mask is looked up as the builtin long slit of its width"):
    forAll: (d: DynamicConfig.GmosNorth) =>
      val expected = d.fpu.map:
        case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(w.northFpu)
        case b                        => b
      val lookup   = SmartGcalRules.gmosNorth.lookupConfig(d)
      assertEquals(lookup, d.copy(fpu = expected).asRight)
      assert(lookup.exists(_.fpu.forall(GmosFpuMask.builtin.getOption(_).isDefined)))

  property("GMOS South: a custom mask is looked up as the builtin long slit of its width"):
    forAll: (d: DynamicConfig.GmosSouth) =>
      val expected = d.fpu.map:
        case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(w.southFpu)
        case b                        => b
      val lookup   = SmartGcalRules.gmosSouth.lookupConfig(d)
      assertEquals(lookup, d.copy(fpu = expected).asRight)
      assert(lookup.exists(_.fpu.forall(GmosFpuMask.builtin.getOption(_).isDefined)))

  property("GMOS: the expanded step's configuration is left alone"):
    forAll: (n: DynamicConfig.GmosNorth, s: DynamicConfig.GmosSouth) =>
      assertEquals(SmartGcalRules.gmosNorth.calibrationConfig(n), n)
      assertEquals(SmartGcalRules.gmosSouth.calibrationConfig(s), s)

  property("Flamingos 2: a custom mask is looked up as the builtin long slit of its width, if it has one"):
    forAll: (d: Flamingos2DynamicConfig) =>
      val lookup = SmartGcalRules.flamingos2.lookupConfig(d)
      d.fpu match
        case Flamingos2FpuMask.Custom(_, w) =>
          w.fpu match
            case Some(f) => assertEquals(lookup, d.copy(fpu = Flamingos2FpuMask.Builtin(f)).asRight)
            case None    => assert(lookup.isLeft, s"expected an error for $w, got $lookup")
        case _                              =>
          assertEquals(lookup, d.asRight)

  // A custom mask of this width must not be searched for as it is, since it
  // would match the entries for configurations without an FPU.
  test("Flamingos 2: the 'Other' custom slit width cannot be looked up"):
    val mask = Flamingos2FpuMask.Custom(ToBeDefined, Flamingos2CustomSlitWidth.Other)
    val d    = Flamingos2DynamicConfig(
      10.secondTimeSpan,
      Flamingos2Disperser.R1200JH.some,
      Flamingos2Filter.JH,
      Flamingos2ReadMode.Bright,
      Flamingos2LyotWheel.F16,
      mask,
      Flamingos2Decker.MOS,
      Flamingos2ReadoutMode.Science,
      Flamingos2ReadMode.Bright.readCount
    )
    assertEquals(
      SmartGcalRules.flamingos2.lookupConfig(d),
      "Flamingos 2 has no builtin FPU equivalent to the 'Other' custom slit width".asLeft
    )

  property("Flamingos 2: a calibration's read mode and reads follow its exposure time"):
    forAll: (d: Flamingos2DynamicConfig) =>
      val mode = Flamingos2ReadMode.forExposureTime(d.exposure)
      assertEquals(SmartGcalRules.flamingos2.calibrationConfig(d), d.copy(readMode = mode, reads = mode.readCount))

  property("GNIRS: the lookup uses the step's own configuration"):
    forAll: (d: GnirsDynamicConfig) =>
      assertEquals(SmartGcalRules.gnirs.lookupConfig(d), d.asRight)

  property("GNIRS: a calibration's read mode follows its exposure time"):
    forAll: (d: GnirsDynamicConfig) =>
      assertEquals(
        SmartGcalRules.gnirs.calibrationConfig(d),
        d.copy(readMode = GnirsReadMode.forExposureTime(d.exposure))
      )
