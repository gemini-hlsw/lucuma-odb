// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.Enumerated

object observationWorkflowState:

  extension (self: ObservationWorkflowState.type)

    def fullSet: Set[ObservationWorkflowState] =
      Enumerated[ObservationWorkflowState].all.toSet

    def preExecutionSet: Set[ObservationWorkflowState] =
      fullSet - self.Ongoing - self.Completed

    def allButComplete: Set[ObservationWorkflowState] =
      fullSet - self.Completed

