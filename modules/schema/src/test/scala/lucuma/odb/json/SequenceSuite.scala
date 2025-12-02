// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.arb.ArbAtom
import lucuma.core.model.sequence.arb.ArbDataset
import lucuma.core.model.sequence.arb.ArbExecutionConfig
import lucuma.core.model.sequence.arb.ArbExecutionSequence
import lucuma.core.model.sequence.arb.ArbInstrumentExecutionConfig
import lucuma.core.model.sequence.arb.ArbSequenceDigest
import lucuma.core.model.sequence.arb.ArbStep
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import lucuma.core.util.arb.ArbGid
import munit.DisciplineSuite
import io.circe.syntax.*
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.TelescopeConfig
import eu.timepit.refined.types.numeric.NonNegInt
import scala.collection.immutable.SortedSet

class SequenceSuite extends DisciplineSuite with ArbitraryInstances:

  import ArbAtom.given
  import ArbDataset.given
  import ArbDynamicConfig.given
  import ArbGid.given
  import ArbInstrumentExecutionConfig.given
  import ArbExecutionConfig.given
  import ArbExecutionSequence.given
  import ArbSequenceDigest.given
  import ArbStaticConfig.given
  import ArbStep.given

  import offset.query.given
  import sequence.given
  import stepconfig.given
  import time.query.given
  import wavelength.query.given
  import gmos.given

  checkAll("Dataset.Filename",             CodecTests[Dataset.Filename].codec)
  checkAll("Dataset.Id",                   CodecTests[Dataset.Id].codec)
  checkAll("SequenceDigest",               CodecTests[SequenceDigest].codec)
  checkAll("Step[GmosNorth]",              CodecTests[Step[DynamicConfig.GmosNorth]].codec)
  checkAll("Atom[GmosNorth]",              CodecTests[Atom[DynamicConfig.GmosNorth]].codec)
  checkAll("ExecutionSequence[GmosNorth]", CodecTests[ExecutionSequence[DynamicConfig.GmosNorth]].codec)
  checkAll("ExecutionConfig[GmosNorth]",   CodecTests[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].codec)
  checkAll("InstrumentExecutionConfig",    CodecTests[InstrumentExecutionConfig].codec)

  test("configs roundtrip"):
    val offset1 = Offset.Zero
    val offset2 = Offset.Zero.copy(q = Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(10)))
    val configs = SortedSet(
      TelescopeConfig(offset1, StepGuideState.Enabled),
      TelescopeConfig(offset1, StepGuideState.Disabled),
      TelescopeConfig(offset2, StepGuideState.Enabled)
    )
    val digest = SequenceDigest(
      ObserveClass.Science,
      CategorizedTime.Zero,
      configs,
      NonNegInt.unsafeFrom(1),
      ExecutionState.Ongoing
    )
    val json = digest.asJson
    // configs are serialized directly
    val configsJson = json.hcursor.downField("configs").as[SortedSet[TelescopeConfig]].toOption.get
    assertEquals(configsJson, configs)
