// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepGuideState.*

trait ToStepGuideStateOps:

  // TODO: Move to lucuma-core
  extension (self: StepGuideState)
    def isGuided: Boolean =
      self match
        case Enabled  => true
        case Disabled => false

object stepguidestate extends ToStepGuideStateOps
