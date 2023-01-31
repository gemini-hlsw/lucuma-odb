// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.FutureExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepTime
import lucuma.core.model.sequence.arb.ArbAtom
import lucuma.core.model.sequence.arb.ArbExecutionSequence
import lucuma.core.model.sequence.arb.ArbFutureExecutionConfig
import lucuma.core.model.sequence.arb.ArbStep
import lucuma.core.model.sequence.arb.ArbStepTime
import munit.DisciplineSuite
import org.scalacheck.Test

class SequenceSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbAtom.*
  import ArbExecutionSequence.*
  import ArbFutureExecutionConfig.*
  import ArbStep.*
  import ArbStepTime.*

  import offset.query.given
  import sequence.given
  import time.query.given
  import wavelength.query.given

  // awaiting lucuma-core 0.64 or better where we limit atoms/sequences to 10 by default
  override val scalaCheckTestParameters: Test.Parameters =
    Test.Parameters.default.withMaxSize(10)

  checkAll("SequenceCodec StepTime",                        CodecTests[StepTime].codec)
  checkAll("SequenceCodec Step GmosNorth",                  CodecTests[Step.GmosNorth].codec)
  checkAll("SequenceCodec Step GmosSouth",                  CodecTests[Step.GmosSouth].codec)
  checkAll("SequenceCodec Atom GmosNorth",                  CodecTests[Atom.GmosNorth].codec)
  checkAll("SequenceCodec Atom GmosSouth",                  CodecTests[Atom.GmosSouth].codec)
  checkAll("SequenceCodec ExecutionSequence GmosNorth",     CodecTests[ExecutionSequence.GmosNorth].codec)
  checkAll("SequenceCodec ExecutionSequence GmosSouth",     CodecTests[ExecutionSequence.GmosSouth].codec)
  checkAll("SequenceCodec FutureExecutionConfig GmosNorth", CodecTests[FutureExecutionConfig.GmosNorth].codec)
  checkAll("SequenceCodec FutureExecutionConfig GmosSouth", CodecTests[FutureExecutionConfig.GmosSouth].codec)
  checkAll("SequenceCodec FutureExecutionConfig",           CodecTests[FutureExecutionConfig].codec)

}
