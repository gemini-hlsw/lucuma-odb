// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.DatasetEstimate
import lucuma.core.model.sequence.DetectorEstimate
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.model.sequence.arb.ArbCategorizedTimeRange
import lucuma.core.model.sequence.arb.ArbConfigChangeEstimate
import lucuma.core.model.sequence.arb.ArbDatasetEstimate
import lucuma.core.model.sequence.arb.ArbDetectorEstimate
import lucuma.core.model.sequence.arb.ArbStepEstimate
import lucuma.core.model.sequence.arb.ArbTimeChargeCorrection
import munit.DisciplineSuite


class TimeAccountingSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbCategorizedTime.given
  import ArbCategorizedTimeRange.given
  import ArbConfigChangeEstimate.given
  import ArbDatasetEstimate.given
  import ArbDetectorEstimate.given
  import ArbStepEstimate.given
  import ArbTimeChargeCorrection.given

  import time.query.given
  import timeaccounting.given

  checkAll("CategorizedTime",      CodecTests[CategorizedTime].codec)
  checkAll("CategorizedTimeRange", CodecTests[CategorizedTimeRange].codec)
  checkAll("ConfigChangeEstimate", CodecTests[ConfigChangeEstimate].codec)
  checkAll("DatasetEstimate",      CodecTests[DatasetEstimate].codec)
  checkAll("DetectorEstimate",     CodecTests[DetectorEstimate].codec)
  checkAll("StepEstimate",         CodecTests[StepEstimate].codec)
  checkAll("TimeChargeCorrection", CodecTests[TimeChargeCorrection].codec)

}
