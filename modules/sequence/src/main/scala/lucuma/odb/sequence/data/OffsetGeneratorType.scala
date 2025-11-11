// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.util.Enumerated

enum OffsetGeneratorType(val tag: String) derives Enumerated:
  case NoGenerator extends OffsetGeneratorType("none")
  case Enumerated  extends OffsetGeneratorType("enumerated")
  case Grid        extends OffsetGeneratorType("grid")
  case Random      extends OffsetGeneratorType("random")
  case Spiral      extends OffsetGeneratorType("spiral")