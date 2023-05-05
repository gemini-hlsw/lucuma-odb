// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Sequence
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.arb.ArbAtom
import lucuma.core.model.sequence.arb.ArbExecutionConfig
import lucuma.core.model.sequence.arb.ArbInstrumentExecutionConfig
import lucuma.core.model.sequence.arb.ArbSequence
import lucuma.core.model.sequence.arb.ArbStep
import lucuma.core.model.sequence.arb.ArbStepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import munit.DisciplineSuite
import org.scalacheck.Test

class SequenceSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbAtom.given
  import ArbDynamicConfig._
  import ArbInstrumentExecutionConfig.given
  import ArbExecutionConfig.given
  import ArbSequence.given
  import ArbStaticConfig._
  import ArbStep.given
  import ArbStepEstimate.given

  import offset.query.given
  import sequence.given
  import time.query.given
  import wavelength.query.given
  import gmos.given

  checkAll("Step[GmosNorth]",      CodecTests[Step[DynamicConfig.GmosNorth]].codec)
  checkAll("Atom[GmosNorth]",      CodecTests[Atom[DynamicConfig.GmosNorth]].codec)
  checkAll("Sequence[GmosNorth]",  CodecTests[Sequence[DynamicConfig.GmosNorth]].codec)
  checkAll("ExecutionConfig[GmosNorth]", CodecTests[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].codec)
  checkAll("InstrumentExecutionConfig",  CodecTests[InstrumentExecutionConfig].codec)

}
