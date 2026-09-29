// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * What a cloned observation takes from its source's materialized sequences.
 */
enum CloneSequenceMode(val tag: String) derives Enumerated:
  case None         extends CloneSequenceMode("none")
  case AllSteps     extends CloneSequenceMode("all_steps")
  case PendingSteps extends CloneSequenceMode("pending_steps")
