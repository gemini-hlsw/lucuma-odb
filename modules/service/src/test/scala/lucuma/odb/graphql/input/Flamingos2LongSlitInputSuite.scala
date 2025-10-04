// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import io.circe.testing.ArbitraryInstances
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.syntax.all.*
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.graphql.input.arb.ArbFlamingos2LongSlitInput.given
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class Flamingos2LongSlitInputSuite extends DisciplineSuite with ArbitraryInstances:

  test("Flamingos2LongSlitInput.Create should have correct observingModeType"):
    forAll: (c: Flamingos2LongSlitInput.Create) =>
      assertEquals(c.observingModeType, ObservingModeType.Flamingos2LongSlit)

  test("Flamingos2LongSlitInput.Edit should have correct observingModeType"):
    forAll: (e: Flamingos2LongSlitInput.Edit) =>
      assertEquals(e.observingModeType, ObservingModeType.Flamingos2LongSlit)

  test("Flamingos2LongSlitInput.Edit toCreate should handle optional values correctly"):
    forAll: (e: Flamingos2LongSlitInput.Edit, grating: Flamingos2Disperser, fpu: Flamingos2Fpu, filter: Flamingos2Filter) =>
      val edit = e.copy(disperser = Some(grating), fpu = Some(fpu), filter = Some(filter))
      val createResult = edit.toCreate

      assert(createResult.toEither.isRight, "Should succeed when all required fields are present")

  test("Flamingos2LongSlitInput.Edit toCreate should fail when required fields are missing"):
    forAll: (edit: Flamingos2LongSlitInput.Edit) =>
      // Case when grating is missing
      val noGrating = edit.copy(disperser = None)
      assert(noGrating.toCreate.isFailure, "Should fail when grating is missing")

      // Case when fpu is missing
      val noFpu = edit.copy(fpu = None)
      assert(noFpu.toCreate.isFailure, "Should fail when fpu is missing")

      // Case when both are missing
      val noBoth = edit.copy(disperser = None, fpu = None)
      assert(noBoth.toCreate.isFailure, "Should fail when both grating and fpu are missing")

  test("OffsetsFormat formattedOffsets works in Create"):
    val offsets = List(
      Offset.Zero.copy(q = 5.arcseconds.q),
      Offset.Zero.copy(q = 10.arcseconds.q)
    )

    val create = Flamingos2LongSlitInput.Create(
      disperser = Flamingos2Disperser.R1200JH,
      filter = Flamingos2Filter.JH,
      fpu = Flamingos2Fpu.LongSlit2,
      explicitOffsets = Some(offsets)
    )

    assertEquals(create.formattedOffsets, Some("0.000000,5.000000,0.000000,10.000000"))

  test("OffsetsFormat formattedOffsets works in Edit"):
    val offsets = List(
      Offset.Zero.copy(q = 5.arcseconds.q),
      Offset.Zero.copy(q = 10.arcseconds.q)
    )

    val edit = Flamingos2LongSlitInput.Edit(
      disperser = None,
      filter = None,
      fpu = None,
      explicitReadMode = lucuma.odb.data.Nullable.Null,
      explicitReads = lucuma.odb.data.Nullable.Null,
      explicitDecker = lucuma.odb.data.Nullable.Null,
      explicitReadoutMode = lucuma.odb.data.Nullable.Null,
      explicitOffsets = lucuma.odb.data.Nullable.NonNull(offsets),
      telluricType = None
    )

    assertEquals(edit.formattedOffsets, lucuma.odb.data.Nullable.NonNull("0.000000,5.000000,0.000000,10.000000"))
