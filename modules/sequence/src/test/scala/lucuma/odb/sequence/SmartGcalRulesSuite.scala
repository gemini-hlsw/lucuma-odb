// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig.given
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig.given
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsDynamicConfig.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class SmartGcalRulesSuite extends ScalaCheckSuite:

  property("GMOS North: a custom mask is looked up as the builtin long slit of its width"):
    forAll: (d: DynamicConfig.GmosNorth) =>
      val expected = d.fpu.map:
        case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(gmos.mos.Config.northFpu(w))
        case b                        => b
      val lookup   = SmartGcalRules.gmosNorth.lookupConfig(d)
      assertEquals(lookup, d.copy(fpu = expected))
      assert(lookup.fpu.forall(GmosFpuMask.builtin.getOption(_).isDefined))

  property("GMOS South: a custom mask is looked up as the builtin long slit of its width"):
    forAll: (d: DynamicConfig.GmosSouth) =>
      val expected = d.fpu.map:
        case GmosFpuMask.Custom(_, w) => GmosFpuMask.Builtin(gmos.mos.Config.southFpu(w))
        case b                        => b
      val lookup   = SmartGcalRules.gmosSouth.lookupConfig(d)
      assertEquals(lookup, d.copy(fpu = expected))
      assert(lookup.fpu.forall(GmosFpuMask.builtin.getOption(_).isDefined))

  property("GMOS: the expanded step's configuration is left alone"):
    forAll: (n: DynamicConfig.GmosNorth, s: DynamicConfig.GmosSouth) =>
      assertEquals(SmartGcalRules.gmosNorth.calibrationConfig(n), n)
      assertEquals(SmartGcalRules.gmosSouth.calibrationConfig(s), s)

  property("Flamingos 2: a custom mask is looked up as the builtin long slit of its width"):
    forAll: (d: Flamingos2DynamicConfig) =>
      val expected = d.fpu match
        case Flamingos2FpuMask.Custom(_, w) => w.fpu.fold(d.fpu)(Flamingos2FpuMask.builtin.reverseGet)
        case m                              => m
      assertEquals(SmartGcalRules.flamingos2.lookupConfig(d), d.copy(fpu = expected))

  property("Flamingos 2: a calibration's read mode and reads follow its exposure time"):
    forAll: (d: Flamingos2DynamicConfig) =>
      val mode = Flamingos2ReadMode.forExposureTime(d.exposure)
      assertEquals(SmartGcalRules.flamingos2.calibrationConfig(d), d.copy(readMode = mode, reads = mode.readCount))

  property("GNIRS: the lookup uses the step's own configuration"):
    forAll: (d: GnirsDynamicConfig) =>
      assertEquals(SmartGcalRules.gnirs.lookupConfig(d), d)

  property("GNIRS: a calibration's read mode follows its exposure time"):
    forAll: (d: GnirsDynamicConfig) =>
      assertEquals(
        SmartGcalRules.gnirs.calibrationConfig(d),
        d.copy(readMode = GnirsReadMode.forExposureTime(d.exposure))
      )
