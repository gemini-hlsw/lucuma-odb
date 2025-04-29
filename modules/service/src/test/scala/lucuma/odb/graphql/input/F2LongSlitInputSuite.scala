// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import io.circe.testing.ArbitraryInstances
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.arb.ArbF2LongSlitInput.given
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class F2LongSlitInputSuite extends DisciplineSuite with ArbitraryInstances:

  test("F2LongSlitInput.Create should have correct observingModeType"):
    forAll: (c: F2LongSlitInput.Create) =>
      assertEquals(c.observingModeType, ObservingModeType.Flamingos2LongSlit)

  test("F2LongSlitInput.Create toObservingMode should preserve values"):
    forAll: (c: F2LongSlitInput.Create) =>
      val om = c.toObservingMode
      assertEquals(om.disperser, c.disperser)
      assertEquals(om.filter, c.filter)
      assertEquals(om.fpu, c.fpu)
      assertEquals(om.explicitReadMode, c.explicitReadMode)
      assertEquals(om.explicitDecker, c.explicitDecker)
      assertEquals(om.explicitReadoutMode, c.explicitReadoutMode)
      assertEquals(om.explicitReads, c.explicitReads)

  test("F2LongSlitInput.Edit should have correct observingModeType"):
    forAll: (e: F2LongSlitInput.Edit) =>
      assertEquals(e.observingModeType, ObservingModeType.Flamingos2LongSlit)

  test("F2LongSlitInput.Edit toCreate should handle optional values correctly"):
    forAll: (e: F2LongSlitInput.Edit, grating: F2Disperser, fpu: F2Fpu, filter: F2Filter) =>
      val edit = e.copy(disperser = Some(grating), fpu = Some(fpu), filter = Some(filter))
      val createResult = edit.toCreate

      assert(createResult.toEither.isRight, "Should succeed when all required fields are present")

  test("F2LongSlitInput.Edit toCreate should fail when required fields are missing"):
    forAll: (edit: F2LongSlitInput.Edit) =>
      // Case when grating is missing
      val noGrating = edit.copy(disperser = None)
      assert(noGrating.toCreate.isFailure, "Should fail when grating is missing")

      // Case when fpu is missing
      val noFpu = edit.copy(fpu = None)
      assert(noFpu.toCreate.isFailure, "Should fail when fpu is missing")

      // Case when both are missing
      val noBoth = edit.copy(disperser = None, fpu = None)
      assert(noBoth.toCreate.isFailure, "Should fail when both grating and fpu are missing")

