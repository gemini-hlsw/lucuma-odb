// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * TODO: Move to lucuma-core.
 */
enum SummaryGenerationState(val tag: String) derives Enumerated:
  case Idle    extends SummaryGenerationState("idle")
  case Pending extends SummaryGenerationState("pending")
  case Failed  extends SummaryGenerationState("failed")
