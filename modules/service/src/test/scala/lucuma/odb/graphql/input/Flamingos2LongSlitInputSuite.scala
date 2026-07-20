// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import io.circe.testing.ArbitraryInstances
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
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

  private val configs = SlitTelescopeConfigs.ToSky(
    NonEmptyList.of(
      TelescopeConfig(Offset.Zero.copy(q =  5.arcseconds.q), StepGuideState.Enabled),
      TelescopeConfig(Offset.Zero.copy(q = 10.arcseconds.q), StepGuideState.Disabled)
    )
  )

  private val configsJson =
    """[{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":5000000}},"guiding":"ENABLED"},{"offset":{"p":{"microarcseconds":0},"q":{"microarcseconds":10000000}},"guiding":"DISABLED"}]"""

  test("formattedTelescopeConfigs works in Create"):
    val create = Flamingos2LongSlitInput.Create(
      disperser = Flamingos2Disperser.R1200JH,
      filter = Flamingos2Filter.JH,
      fpu = Flamingos2Fpu.LongSlit2,
      exposureTimeMode = None,
      explicitTelescopeConfigs = Some(configs),
      acquisition = None
    )

    assertEquals(create.formattedTelescopeConfigs, Some(configsJson))
    assertEquals(create.explicitSlitOffsetMode, Some(SlitOffsetMode.NodToSky))

  test("formattedTelescopeConfigs works in Edit"):
    val edit = Flamingos2LongSlitInput.Edit(
      disperser = None,
      filter = None,
      fpu = None,
      exposureTimeMode = None,
      explicitReadMode = lucuma.odb.data.Nullable.Null,
      explicitReads = lucuma.odb.data.Nullable.Null,
      explicitDecker = lucuma.odb.data.Nullable.Null,
      explicitReadoutMode = lucuma.odb.data.Nullable.Null,
      explicitTelescopeConfigs = lucuma.odb.data.Nullable.NonNull(configs),
      telluricType = Some(TelluricType.Hot),
      acquisition = None
    )

    assertEquals(edit.formattedTelescopeConfigs, lucuma.odb.data.Nullable.NonNull(configsJson))
    assertEquals(edit.explicitSlitOffsetMode, lucuma.odb.data.Nullable.NonNull(SlitOffsetMode.NodToSky))
