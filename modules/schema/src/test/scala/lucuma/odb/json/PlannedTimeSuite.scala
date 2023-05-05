// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.arb.ArbConfigChangeEstimate
import lucuma.core.model.sequence.arb.ArbDatasetEstimate
import lucuma.core.model.sequence.arb.ArbDetectorEstimate
import lucuma.core.model.sequence.arb.ArbPlannedTime
import lucuma.core.model.sequence.arb.ArbStepEstimate
import munit.DisciplineSuite


class PlannedTimeSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbConfigChangeEstimate.given
  import ArbDatasetEstimate.given
  import ArbDetectorEstimate.given
  import ArbPlannedTime.given
  import ArbStepEstimate.given

  import plannedtime.given
  import time.query.given

  checkAll("ConfigChangeEstimate", CodecTests[ConfigChangeEstimate].codec)
  checkAll("DatasetEstimate",      CodecTests[DatasetEstimate].codec)
  checkAll("DetectorEstimate",     CodecTests[DetectorEstimate].codec)
  checkAll("PlannedTime",          CodecTests[PlannedTime].codec)
  checkAll("StepEstimate",         CodecTests[StepEstimate].codec)

}
