// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.odb.format.spatialOffsets.*
import munit.DisciplineSuite

class SpatialOffsetsFormatSuite extends DisciplineSuite:
  checkAll("SpatialOffsetFormat.OffsetsQ", FormatTests(OffsetsQFormat).formatLaws)
  checkAll("SpatialOffsetFormat.OffsetsP", FormatTests(OffsetsPFormat).formatLaws)
  checkAll("SpatialOffsetFormat.Offsets", FormatTests(OffsetsFormat).formatLaws)
